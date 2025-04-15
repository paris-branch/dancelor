open Nes
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
        if not @@ HashedPassword.is ~clear: password (Model.User.password user) then
          (
            Log.info (fun m -> m "Rejecting because passwords do not match.");
            Lwt.return_none
          )
        else
          (
            Log.info (fun m -> m "Accepting login.");
            ignore @@ Environment.set_session_user env (Some user);
            Lwt.map Option.some @@ Model.User.person user
          )

let logout env =
  ignore @@ Environment.set_session_user env None;
  Lwt.return_unit

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Auth.t -> a = fun env endpoint ->
  match endpoint with
  | Status -> status env
  | Login -> login env
  | Logout -> logout env
