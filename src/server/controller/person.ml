open Nes
open Dancelor_common

let get env id =
  match%lwt Database.Person.get id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get_public env person;%lwt
    lwt person

let for_user env id =
  match%lwt Database.User.get_person id with
  | None -> lwt_none
  | Some person_id ->
    match%lwt Database.Person.get person_id with
    | None -> lwt_none
    | Some person ->
      if%lwt Permission.can_get_public env person then
        lwt_some person
      else
        lwt_none

let create env person =
  Permission.assert_can_create_public env;%lwt
  let%lwt id = Database.Person.create person in
  Option.get <$> Database.Person.get id

let update env id person =
  Permission.assert_can_update_public env =<< get env id;%lwt
  ignore <$> Database.Person.update id person;%lwt
  Option.get <$> Database.Person.get id

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Person.delete id

include Search.Build(struct
  type value = Model.Person.entry
  type filter = (Model.Person.t, Filter.Person.t) Formula_entry.public

  let get_all env =
    let all = Database.Person.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Person.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Person.accepts
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
