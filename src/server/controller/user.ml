open NesUnix
open Common

module Log = (val Logger.create "controller.user": Logs.LOG)

let status = lwt % Environment.user

let sign_in env username password remember_me =
  Log.info (fun m -> m "Attempt to sign in with username `%s`." (Slug.to_string username));
  match Environment.user env with
  | Some _user ->
    Log.info (fun m -> m "Rejecting because already signed in.");
    lwt_none
  | None ->
    match%lwt Database.User.get_opt username with
    | None ->
      Log.info (fun m -> m "Rejecting because of wrong username.");
      lwt_none
    | Some user ->
      match Model.User.password' user with
      | None ->
        Log.info (fun m -> m "Rejecting because user has no password.");
        lwt_none
      | Some hashedPassword when not @@ HashedSecret.is ~clear: password hashedPassword ->
        Log.info (fun m -> m "Rejecting because passwords do not match.");
        lwt_none
      | Some _ ->
        Environment.sign_in env user ~remember_me;%lwt
        Log.info (fun m -> m "Accepted sign in for %a." Environment.pp env);
        lwt_some user

let sign_out env =
  match Environment.user env with
  | None -> lwt_unit
  | Some user -> Environment.sign_out env user

let create env username user =
  Permission.assert_can_admin env;%lwt
  match Slug.check username with
  | false -> Madge_server.shortcut_bad_request "The username does not have the right shape."
  | true ->
    let token = uid () in
    let hashed_token = (HashedSecret.make ~clear: token, Datetime.make_in_the_future (float_of_int @@ 3 * 24 * 3600)) in
    let%lwt user = Model.User.update user ~password_reset_token: (const @@ Some hashed_token) in
    let%lwt user = Database.User.create_with_slug username user in
    lwt (user, token)

let reset_password username token password =
  Log.info (fun m -> m "Attempt to reset password for user `%a`." Slug.pp' username);
  match Slug.check username with
  | false ->
    Log.info (fun m -> m "Rejecting because username is not even a slug.");
    Madge_server.shortcut_bad_request "The username does not have the right shape."
  | true ->
    match%lwt Database.User.get_opt username with
    | None ->
      Log.info (fun m -> m "Rejecting because of wrong username.");
      Madge_server.shortcut_forbidden_no_leak ()
    | Some user ->
      match Model.User.password_reset_token' user with
      | None ->
        Log.info (fun m -> m "Rejecting because of lack of token.");
        Madge_server.shortcut_forbidden_no_leak ()
      | Some (_, token_date) when Datetime.(diff (now ())) token_date > 3. *. 24. *. 3600. ->
        Log.info (fun m -> m "Rejecting because token is too old.");
        Madge_server.shortcut_forbidden_no_leak ()
      | Some (hashed_token, _) ->
        if not @@ HashedSecret.is ~clear: token hashed_token then
          (
            Log.info (fun m -> m "Rejecting because tokens do no match.");
            Madge_server.shortcut_forbidden_no_leak ()
          )
        else
          (
            Log.info (fun m -> m "Accepting to reset password.");
            let password = HashedSecret.make ~clear: password in
            ignore
            <$> Database.User.update
                (Entry.slug user)
                {(Entry.value user) with
                  password = Some password;
                  password_reset_token = None;
                };%lwt
            lwt_unit
          )

let can_create env = lwt @@ Permission.can_create env
let can_admin env = lwt @@ Permission.can_admin env

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.User.t -> a = fun env endpoint ->
  match endpoint with
  | Status -> status env
  | SignIn -> sign_in env
  | SignOut -> sign_out env
  | Create -> create env
  | ResetPassword -> reset_password
  | CanCreate -> can_create env
  | CanAdmin -> can_admin env
