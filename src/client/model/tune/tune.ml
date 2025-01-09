open Dancelor_common
include TuneLifted

let make_and_save
    ?status
    ~name
    ?alternative_names
    ~kind
    ?composers
    ?dances
    ?remark
    ?scddb_id
    ?date
    ~modified_at
    ~created_at
    ()
  =
  Madge_client_new.call
    ApiRouter.(route @@ Tune MakeAndSave)
    status
    name
    alternative_names
    kind
    composers
    dances
    remark
    scddb_id
    date
    modified_at
    created_at

let search ?slice ?threshold filter =
  Madge_client_new.call ApiRouter.(route @@ Tune Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
