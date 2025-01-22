open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include TuneLifted

let create = Database.Tune.create
let update = Database.Tune.update
let save = Database.Tune.save

let tiebreakers =
  Lwt_list.[
    increasing (Lwt.return % name) String.Sensible.compare;
    increasing (Lwt.return % name) String.compare_lengths;
  ]

let search =
  Common.Model.Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Tune.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
