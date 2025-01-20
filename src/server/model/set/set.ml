open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include SetLifted

let save ?status ~modified_at ~created_at set =
  Database.Set.save ~slug_hint: set.name ?status ~modified_at ~created_at set

let delete = Database.(Set.delete % Entry.slug)

let tiebreakers =
  Lwt_list.[
    increasing (Lwt.return % name) String.Sensible.compare;
    increasing (Lwt.return % name) String.compare_lengths;
  ]

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Set.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
