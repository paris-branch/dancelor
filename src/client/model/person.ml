open Nes
open Common

include ModelBuilder.Person.Build ()

let get = Madge_client.call Endpoints.Api.(route @@ Person Get)

let create = Madge_client.call Endpoints.Api.(route @@ Person Create)
let update = Madge_client.call Endpoints.Api.(route @@ Person Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_client.call Endpoints.Api.(route @@ Person Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
