open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include TuneLifted

let save
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
  Database.Tune.save ~slug_hint: name @@ fun slug ->
  make
    ?status
    ~slug
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

let tiebreakers =
  Lwt_list.[
    increasing (Lwt.return % name) String.Sensible.compare;
    increasing (Lwt.return % name) String.compare_lengths;
  ]

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Tune.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
