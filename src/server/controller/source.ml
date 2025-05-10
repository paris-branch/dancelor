open Nes
open Common

let get env slug =
  Lwt.bind_return
    (Model.Source.get slug)
    (Permission.assert_can_get env)

let create env source =
  Permission.assert_can_create env;%lwt
  Database.Source.create source

let update env slug source =
  Lwt.bind (get env slug) (Permission.assert_can_update env);%lwt
  Database.Source.update slug source

include Search.Build(struct
  type value = Model.Source.t Entry.t
  type filter = Model.Source.Filter.t

  let get_all env =
    Lwt.map
      (List.filter (Permission.can_get env))
      (Database.Source.get_all ())

  let filter_accepts = Model.Source.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Source.name') String.Sensible.compare]
end)

let get_cover env slug =
  Lwt.bind (get env slug) (Permission.assert_can_get env);%lwt
  Database.Source.with_cover slug @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat !Config.share "no-cover.webp") in
  Madge_server.respond_file ~content_type: "image/webp" ~fname

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Cover -> get_cover env
