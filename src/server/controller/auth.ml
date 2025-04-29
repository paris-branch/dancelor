open NesUnix
open Common

module Log = (val Logger.create "controller.auth": Logs.LOG)

let status = Lwt.return % Environment.user

let sign_in env username password remember_me =
  Log.info (fun m -> m "Attempt to sign in with username `%s`." username);
  match Environment.user env with
  | Some _user ->
    Log.info (fun m -> m "Rejecting because already signed in.");
    Lwt.return_none
  | None ->
    match Slug.check_string username with
    | None ->
      Log.info (fun m -> m "Rejecting because username is not even a slug.");
      Lwt.return_none
    | Some username ->
      match%lwt Database.User.get_opt username with
      | None ->
        Log.info (fun m -> m "Rejecting because of wrong username.");
        Lwt.return_none
      | Some user ->
        match Model.User.password user with
        | None ->
          Log.info (fun m -> m "Rejecting because user has no password.");
          Lwt.return_none
        | Some hashedPassword when not @@ HashedSecret.is ~clear: password hashedPassword ->
          Log.info (fun m -> m "Rejecting because passwords do not match.");
          Lwt.return_none
        | Some _ ->
          Environment.sign_in env user ~remember_me;%lwt
          Log.info (fun m -> m "Accepted sign in for %a." Environment.pp env);
          Lwt.return_some user

let sign_out env =
  match Environment.user env with
  | None -> Lwt.return_unit
  | Some user -> Environment.sign_out env user

let create_user env username person =
  Permission.assert_can_admin env;%lwt
  let%lwt person = Person.get env person in
  match Slug.check_string username with
  | None -> Madge_server.respond_bad_request "The username does not have the right shape."
  | Some username ->
    let token = uid () in
    let hashed_token = (HashedSecret.make ~clear: token, Datetime.make_in_the_future (float_of_int @@ 3 * 24 * 3600)) in
    let%lwt _user =
      Database.User.create_with_slug username @@
        Model.User.make
          ~person
          ~password_reset_token: hashed_token
          ()
    in
    Lwt.return token

let reset_password username token password =
  Log.info (fun m -> m "Attempt to reset password for user `%s`." username);
  match Slug.check_string username with
  | None ->
    Log.info (fun m -> m "Rejecting because username is not even a slug.");
    Madge_server.respond_bad_request "The username does not have the right shape."
  | Some username ->
    match%lwt Database.User.get_opt username with
    | None ->
      Log.info (fun m -> m "Rejecting because of wrong username.");
      Madge_server.respond_forbidden_no_leak ()
    | Some user ->
      match Model.User.password_reset_token user with
      | None ->
        Log.info (fun m -> m "Rejecting because of lack of token.");
        Madge_server.respond_forbidden_no_leak ()
      | Some (_, token_date) when Datetime.(diff (now ())) token_date > 3. *. 24. *. 3600. ->
        Log.info (fun m -> m "Rejecting because token is too old.");
        Madge_server.respond_forbidden_no_leak ()
      | Some (hashed_token, _) ->
        if not @@ HashedSecret.is ~clear: token hashed_token then
          (
            Log.info (fun m -> m "Rejecting because tokens do no match.");
            Madge_server.respond_forbidden_no_leak ()
          )
        else
          (
            Log.info (fun m -> m "Accepting to reset password.");
            let password = HashedSecret.make ~clear: password in
            Lwt.map ignore @@
              Database.User.update
                (Entry.slug user)
                {(Entry.value user) with
                  password = Some password;
                  password_reset_token = None;
                };%lwt
            Lwt.return_unit
          )

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Auth.t -> a = fun env endpoint ->
  match endpoint with
  | Status -> status env
  | SignIn -> sign_in env
  | SignOut -> sign_out env
  | CreateUser -> create_user env
  | ResetPassword -> reset_password
