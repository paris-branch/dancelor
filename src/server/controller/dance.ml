open NesUnix
open Common

module Log = (val Logger.create "controller.dance": Logs.LOG)

let get env id =
  match Database.Dance.get id with
  | None -> Permission.reject_can_get ()
  | Some dance ->
    Permission.assert_can_get env dance;%lwt
    lwt dance

let create env dance =
  Permission.assert_can_create env;%lwt
  Database.Dance.create dance

let update env id dance =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Dance.update id dance

include Search.Build(struct
  type value = Model.Dance.t Entry.t
  type filter = Filter.Dance.t

  let get_all env =
    List.filter (Permission.can_get env) (Database.Dance.get_all ())

  let filter_accepts = Filter.Dance.accepts

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Dance.one_name') String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Dance.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
