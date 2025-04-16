open Nes
open Common

let get = Model.Source.get

let create = Database.Source.create
let update = Database.Source.update
let save = Database.Source.save

include ModelBuilder.Search.Build(struct
  type value = Model.Source.t Entry.t
  type filter = Model.Source.Filter.t

  let cache = Cache.create ~lifetime: 600 ()
  let get_all = Database.Source.get_all
  let filter_accepts = Model.Source.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Source.name) String.Sensible.compare]
end)

let get_cover source =
  Madge_cohttp_lwt_server.shortcut @@
  Database.Source.with_cover source @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat !Config.share "no-cover.webp") in
  Cohttp_lwt_unix.Server.respond_file ~fname ()

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> get
  | Search -> search
  | Create -> create
  | Update -> update
  | Cover -> get_cover
