open Dancelor_common
include SetLifted

let save
    ?status
    ~name
    ?conceptors
    ~kind
    ?contents
    ~order
    ?dances
    ~modified_at
    ~created_at
    ()
  =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Set Save)
    status
    name
    conceptors
    kind
    contents
    order
    dances
    modified_at
    created_at

let delete s = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Delete) (slug s)

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
