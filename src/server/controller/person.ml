open Nes
open Common

let get env id =
  let%lwt person = Model.Person.get id in
  Permission.assert_can_get env person;%lwt
  lwt person

let create env person =
  Permission.assert_can_create env;%lwt
  Database.Person.create person

let update env id person =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Person.update id person

include Search.Build(struct
  type value = Model.Person.t Entry.t
  type filter = Filter.Person.t

  let get_all env =
    List.filter (Permission.can_get env)
    <$> Database.Person.get_all ()

  let filter_accepts = Filter.Person.accepts

  let tiebreakers =
    Lwt_list.[increasing (lwt % Model.Person.name') String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
