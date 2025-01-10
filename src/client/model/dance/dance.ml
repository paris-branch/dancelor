open Dancelor_common
include DanceLifted

let make_and_save
    ?status
    ~name
    ~kind
    ?devisers
    ?two_chords
    ?scddb_id
    ?disambiguation
    ?date
    ~modified_at
    ~created_at
    ()
  =
  Madge_client_new.call
    ApiRouter.(route @@ Dance MakeAndSave)
    status
    name
    kind
    devisers
    two_chords
    scddb_id
    disambiguation
    date
    modified_at
    created_at

let search ?slice ?threshold filter =
  Madge_client_new.call ApiRouter.(route @@ Dance Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
