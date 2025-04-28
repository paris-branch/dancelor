open Nes
open Common

include ModelBuilder.Set.Build(Dance)(Person)(Tune)(Version)

let get = Madge_client.call Endpoints.Api.(route @@ Set Get)

let create = Madge_client.call Endpoints.Api.(route @@ Set Create)
let update = Madge_client.call Endpoints.Api.(route @@ Set Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let delete s = Madge_client.call Endpoints.Api.(route @@ Set Delete) (Entry.slug s)

let search = Madge_client.call Endpoints.Api.(route @@ Set Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
