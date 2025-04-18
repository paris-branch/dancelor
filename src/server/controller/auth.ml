open NesUnix
open Common

module Log = (val Logger.create "controller.auth": Logs.LOG)

let status env =
  Lwt_option.map' Model.User.person Environment.(user @@ session env)

let login env username password =
  Log.info (fun m -> m "Attempt to login with username `%s`." username);
  match Environment.(user @@ session env) with
  | Some _user ->
    Log.info (fun m -> m "Rejecting because already logged in.");
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
        (
          match Model.User.password user with
          | None ->
            Log.info (fun m -> m "Rejecting because user has no password.");
            Lwt.return_none
          | Some hashedPassword when not @@ HashedPassword.is ~clear: password hashedPassword ->
            Log.info (fun m -> m "Rejecting because passwords do not match.");
            Lwt.return_none
          | Some _ ->
            Log.info (fun m -> m "Accepting login.");
            ignore @@ Environment.set_session_user env (Some user);
            Lwt.map Option.some @@ Model.User.person user
        )

let logout env =
  ignore @@ Environment.set_session_user env None;
  Lwt.return_unit

let reset_password username token password =
  Log.info (fun m -> m "Attempt to reset password for user `%s`." username);
  match Slug.check_string username with
  | None ->
    Log.info (fun m -> m "Rejecting because username is not even a slug.");
    Madge_cohttp_lwt_server.shortcut' `Bad_request
  | Some username ->
    match%lwt Database.User.get_opt username with
    | None ->
      Log.info (fun m -> m "Rejecting because of wrong username.");
      Madge_cohttp_lwt_server.shortcut' `Forbidden
    | Some user ->
      (
        match Model.User.password_reset_token (Entry.value user) with
        | None ->
          Log.info (fun m -> m "Rejecting because of lack of token.");
          Madge_cohttp_lwt_server.shortcut' `Forbidden
        | Some (token', _) when token' <> token ->
          Log.info (fun m -> m "Rejecting because tokens do no match.");
          Madge_cohttp_lwt_server.shortcut' `Forbidden
        | Some (_, token_date) when Datetime.(diff (now ())) token_date > 3. *. 24. *. 3600. ->
          Log.info (fun m -> m "Rejecting because token is too old.");
          Madge_cohttp_lwt_server.shortcut' `Forbidden
        | Some _ ->
          (
            Log.info (fun m -> m "Accepting to reset password.");
            let password = HashedPassword.make ~clear: password in
            Lwt.map ignore @@
              Database.User.update
                (Entry.slug user)
                {(Entry.value user) with
                  password = Some password;
                  password_reset_token = None;
                };%lwt
            Lwt.return_unit
          )
      )

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Auth.t -> a = fun env endpoint ->
  match endpoint with
  | Status -> status env
  | Login -> login env
  | Logout -> logout env
  | ResetPassword -> reset_password
