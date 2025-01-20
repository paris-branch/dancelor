open Dancelor_common
include PersonLifted

let save ?status ~modified_at ~created_at person =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Person Save)
    status
    modified_at
    created_at
    person

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Person Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
