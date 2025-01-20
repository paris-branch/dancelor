open Dancelor_common
include TuneLifted

let save
    ?status
    ~modified_at
    ~created_at
    tune
  =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Tune Save)
    status
    modified_at
    created_at
    tune

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Tune Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
