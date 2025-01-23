open Nes
open Dancelor_common

include VersionLifted

let create = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Version Create)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Version Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Version Search)
let search' = Lwt.map snd % search Model.Slice.everything
let count = Lwt.map fst % search Model.Slice.nothing
