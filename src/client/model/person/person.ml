open Dancelor_common
include PersonLifted

let save ?status ~name ?scddb_id ~modified_at ~created_at () =
  Madge_client_new.call
    ApiRouter.(route @@ Person Save)
    status
    name
    scddb_id
    modified_at
    created_at

let search ?slice ?threshold filter =
  Madge_client_new.call ApiRouter.(route @@ Person Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
