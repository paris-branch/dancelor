open Nes
open Dancelor_common

include Dancelor_common.Model.Dance.Lift(Person)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Get)

let create = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Create)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
