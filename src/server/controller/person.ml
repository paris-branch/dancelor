open Nes
open Common

let get env id =
  match Database.Person.get id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get env person;%lwt
    lwt person

let create env person =
  Permission.assert_can_create env;%lwt
  Database.Person.create person

let update env id person =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Person.update id person

let delete env id =
  Permission.assert_can_delete env =<< get env id;%lwt
  Database.Person.delete id

include Search.Build(struct
  type value = Model.Person.t Entry.t
  type filter = Filter.Person.t

  let get_all env =
    Lwt_stream.filter (Permission.can_get env) @@ Lwt_stream.of_seq @@ Database.Person.get_all ()

  let optimise_filter = Filter.Person.optimise
  let filter_is_empty = (=) Formula.False
  let filter_is_full = (=) Formula.True
  let filter_accepts = Filter.Person.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Person.name') String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
