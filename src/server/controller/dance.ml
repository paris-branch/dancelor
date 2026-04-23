open NesUnix
open Dancelor_common

module Log = (val Logs.src_log @@ Logs.Src.create "server.controller.dance": Logs.LOG)

let get env id =
  match%lwt Database.Dance.get id with
  | None -> Permission.reject_can_get ()
  | Some dance ->
    Permission.assert_can_get_public env dance;%lwt
    lwt dance

let create env dance =
  Permission.assert_can_create_public env;%lwt
  Database.Dance.create dance

let update env id dance =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Dance.update id dance

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Dance.delete id

include Search.Build(struct
  type value = Model.Dance.entry
  type filter = (Model.Dance.t, Filter.Dance.t) Formula_entry.public

  let get_all env =
    let all = Database.Dance.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Dance.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Dance.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Dance.one_name') String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Dance.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
