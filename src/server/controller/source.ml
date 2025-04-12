open Nes
open Common

let get_cover source =
  Madge_cohttp_lwt_server.shortcut @@
  Database.Source.with_cover source @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat !Config.share "no-cover.webp") in
  Cohttp_lwt_unix.Server.respond_file ~fname ()

let dispatch : type a r. (a, r Lwt.t, r) Endpoints.Source.t -> a = function
  | Get -> Model.Source.get
  | Search -> Model.Source.search
  | Create -> Model.Source.create
  | Update -> Model.Source.update
  | Cover -> get_cover
