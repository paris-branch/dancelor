open Nes
open Common

let get env id =
  match Database.Person.get id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get_public env person;%lwt
    lwt person

(** FIXME: This is really the ugliest hack. Basically, we are missing more and
    more a good database system; this should be an easy query. *)
let for_user env id =
  let%lwt person =
    Database.Person.get_all ()
    |> Monadise_lwt.monadise_1_1 Seq.find (fun person ->
        match%lwt Model.Person.user' person with
        | None -> lwt_false
        | Some user -> lwt @@ Entry.Id.equal' (Entry.id user) id
      )
  in
  match person with
  | None -> lwt_none
  | Some person ->
    match Database.Person.get (Entry.id person) with
    | None -> lwt_none
    | Some person ->
      if Permission.can_get_public env person then
        lwt_some person
      else lwt_none

let create env person =
  Permission.assert_can_create_public env;%lwt
  Database.Person.create person Entry.Access.Public

let update env id person =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Person.update id person Entry.Access.Public

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Person.delete id

include Search.Build(struct
  type value = Model.Person.entry
  type filter = Filter.Person.t

  let get_all env =
    Lwt_stream.filter (Permission.can_get_public env) @@ Lwt_stream.of_seq @@ Database.Person.get_all ()

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
  | For_user -> for_user env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
