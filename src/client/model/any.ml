open Nes
open Dancelor_common

include Dancelor_common.Model.Any.Lift(Book)(Dance)(Person)(Set)(Tune)(Version)

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Any Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing

let search_context = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Any SearchContext)
