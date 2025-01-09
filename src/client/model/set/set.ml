open Dancelor_common
include SetLifted

let make_and_save
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
  Madge_client_new.call
    ApiRouter.(route @@ Set MakeAndSave)
    status
    name
    conceptors
    kind
    contents
    order
    dances
    modified_at
    created_at

let delete s = Madge_client_new.call ApiRouter.(route @@ Set Delete) (slug s)

let search ?slice ?threshold filter =
  Madge_client_new.call ApiRouter.(route @@ Set Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
