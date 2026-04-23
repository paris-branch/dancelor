open Nes
open Dancelor_common

let get env id =
  match%lwt Database.Source.get id with
  | None -> Permission.reject_can_get ()
  | Some source ->
    Permission.assert_can_get_public env source;%lwt
    lwt source

let create env source =
  Permission.assert_can_create_public env;%lwt
  Database.Source.create source

let update env id source =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Source.update id source

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Source.delete id

include Search.Build(struct
  type value = Model.Source.entry
  type filter = (Model.Source.t, Filter.Source.t) Formula_entry.public

  let get_all env =
    let all = Database.Source.get_all () in
    let stream = (Lwt_stream.filter (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Source.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Source.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Source.name') String.Sensible.compare]
end)

let get_cover env id =
  Permission.assert_can_get_public env =<< get env id;%lwt
  Database.Source.with_cover id @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat (Config.get ()).share "no-cover.webp") in
  Madge_server.respond_file ~fname

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Cover -> get_cover env
