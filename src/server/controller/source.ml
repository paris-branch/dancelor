open Nes
open Common

let get env id =
  match Database.Source.get id with
  | None -> Permission.reject_can_get ()
  | Some source ->
    Permission.assert_can_get_public env source;%lwt
    lwt source

let create env source =
  Permission.assert_can_create_public env;%lwt
  Database.Source.create source Entry.Access.Public

let update env id source =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Source.update id source Entry.Access.Public

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Source.delete id

include Search.Build(struct
  type value = Model.Source.entry
  type filter = Filter.Source.t

  let get_all env =
    Lwt_stream.filter (Permission.can_get_public env) @@ Lwt_stream.of_seq @@ Database.Source.get_all ()

  let optimise_filter = Filter.Source.optimise
  let filter_is_empty = (=) Formula.False
  let filter_is_full = (=) Formula.True
  let filter_accepts = Filter.Source.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[decreasing (lwt % Model.Source.date') (Option.compare PartialDate.compare);
    increasing (lwt % NEString.to_string % Model.Source.name') String.Sensible.compare;
    ]
end)

let get_cover env id =
  Permission.assert_can_get_public env =<< get env id;%lwt
  Database.Source.with_cover id @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat !Config.share "no-cover.webp") in
  Madge_server.respond_file ~fname

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Cover -> get_cover env
