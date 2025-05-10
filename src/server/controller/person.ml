open Nes
open Common

let get env slug =
  Lwt.bind_return
    (Model.Person.get slug)
    (Permission.assert_can_get env)

let create env person =
  Permission.assert_can_create env;%lwt
  Database.Person.create person

let update env slug person =
  Lwt.bind (get env slug) (Permission.assert_can_update env);%lwt
  Database.Person.update slug person

include Search.Build(struct
  type value = Model.Person.t Entry.t
  type filter = Model.Person.Filter.t

  let get_all env =
    Lwt.map
      (List.filter (Permission.can_get env))
      (Database.Person.get_all ())

  let filter_accepts = Model.Person.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Person.name') String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
