open Dancelor_common
include DanceLifted

let save
    ?status
    ~modified_at
    ~created_at
    dance
  =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Dance Save)
    status
    modified_at
    created_at
    dance

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
