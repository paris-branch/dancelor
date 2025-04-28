open Nes
open Common

include ModelBuilder.Dance.Build(Person)

let get = Madge_client.call Endpoints.Api.(route @@ Dance Get)

let create = Madge_client.call Endpoints.Api.(route @@ Dance Create)
let update = Madge_client.call Endpoints.Api.(route @@ Dance Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_client.call Endpoints.Api.(route @@ Dance Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
