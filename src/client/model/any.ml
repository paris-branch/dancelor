open Nes
open Dancelor_common

include Model.Any.Lift(Book)(Dance)(Person)(Set)(Tune)(Version)

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Any Search)
let search' = Lwt.map snd % search Model.Slice.everything
let count = Lwt.map fst % search Model.Slice.nothing

let search_context = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Any SearchContext)
