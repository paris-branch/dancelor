open Dancelor_common
include BookLifted

let save
    ?status
    ~modified_at
    ~created_at
    book
  =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Book Save)
    status
    modified_at
    created_at
    book

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let update
    ?status
    ~modified_at
    ~created_at
    slug
    book
  =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Book Update)
    status
    modified_at
    created_at
    slug
    book
