open Nes
open Common

include ModelBuilder.Any.Build(Book)(Dance)(Person)(Set)(Source)(Tune)(Version)

let search = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Any Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing

let search_context = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Any SearchContext)
