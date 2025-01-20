open Dancelor_common
include SetLifted

let save
    ?status
    ~modified_at
    ~created_at
    set
  =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Set Save)
    status
    modified_at
    created_at
    set

let delete s = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Delete) (Database.Entry.slug s)

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
