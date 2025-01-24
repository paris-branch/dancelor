open Nes
open Dancelor_common

include Model.Tune.Lift(Dance)(Person)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Tune Get)

let create = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Tune Create)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Tune Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Tune Search)
let search' = Lwt.map snd % search Model.Slice.everything
let count = Lwt.map fst % search Model.Slice.nothing
