open Nes
open Common

include ModelBuilder.Tune.Build(Dance)(Person)

let get = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Tune Get)

let create = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Tune Create)
let update = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Tune Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Tune Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
