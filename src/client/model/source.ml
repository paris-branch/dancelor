open Nes
open Common

include ModelBuilder.Source.Build ()

let get = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Source Get)

let create = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Source Create)
let update = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Source Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Source Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
