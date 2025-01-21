open Dancelor_common
include BookLifted

let save = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Save)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Update)

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
