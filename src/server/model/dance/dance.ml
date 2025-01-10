open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

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
  Database.Dance.save ~slug_hint: name @@ fun slug ->
  make
    ?status
    ~slug
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

let tiebreakers =
  Lwt_list.[
    increasing (Lwt.return % name) String.Sensible.compare
  ]

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Dance.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
