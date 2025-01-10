open Dancelor_common
include AnyLifted

let search ?slice ?threshold filter =
  Madge_client_new.call ApiRouter.(route @@ Any Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let search_context ?threshold filter element =
  Madge_client_new.call ApiRouter.(route @@ Any SearchContext) threshold filter element
