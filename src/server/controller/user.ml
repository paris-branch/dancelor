open Nes
open Common

module Log = (val Logger.create "controller.user": Logs.LOG)

module Auth = struct
  module Log = (val Logger.create "controller.user.auth": Logs.LOG)

  let status env =
    Lwt_option.map' Model.User.person Environment.(user @@ session env)

  let login env username password =
    Log.debug (fun m -> m "Attempt to login with username `%s`." username);
    match Environment.(user @@ session env) with
    | Some _user ->
      Log.debug (fun m -> m "Rejecting because already logged in.");
      Lwt.return_none
    | None ->
      Lwt_option.bind' (Slug.check_string username) @@ fun username ->
      Lwt_option.bind (Database.User.get_opt username) @@ fun user ->
      if not @@ HashedPassword.is ~clear: password (Model.User.password user) then
        (
          Log.debug (fun m -> m "Rejecting because passwords do not match.");
          Lwt.return_none
        )
      else
        (
          ignore @@ Environment.set_session_user env (Some user);
          Lwt.map Option.some @@ Model.User.person user
        )

  let logout env =
    ignore @@ Environment.set_session_user env None;
    Lwt.return_unit
end

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.User.t -> a = fun env endpoint ->
  match endpoint with
  | Auth Status -> Auth.status env
  | Auth Login -> Auth.login env
  | Auth Logout -> Auth.logout env
