open Nes
open Common

let get env id =
  let%lwt source = Model.Source.get id in
  Permission.assert_can_get env source;%lwt
  lwt source

let create env source =
  Permission.assert_can_create env;%lwt
  Database.Source.create source

let update env id source =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Source.update id source

include Search.Build(struct
  type value = Model.Source.t Entry.t
  type filter = Filter.Source.t

  let get_all env =
    List.filter (Permission.can_get env) <$> Database.Source.get_all ()

  let filter_accepts = Filter.Source.accepts

  let tiebreakers =
    Lwt_list.[increasing (lwt % Model.Source.name') String.Sensible.compare]
end)

let get_cover env id =
  Permission.assert_can_get env =<< get env id;%lwt
  Database.Source.with_cover id @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat !Config.share "no-cover.webp") in
  Madge_server.respond_file ~content_type: "image/webp" ~fname

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Cover -> get_cover env
