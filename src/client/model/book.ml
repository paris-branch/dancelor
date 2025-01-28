open Nes
open Dancelor_common

include Dancelor_common.Model.Book.Lift(Dance)(Set)(Tune)(Version)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Get)

let create = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Create)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
