open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include BookLifted

let create = Database.Book.create
let update = Database.Book.update
let save = Database.Book.save

let tiebreakers =
  Lwt_list.[
    decreasing (Lwt.return % date) (NesOption.compare NesPartialDate.compare);
    increasing (Lwt.return % title) String.Sensible.compare;
    increasing (Lwt.return % title) String.compare_lengths;
    increasing (Lwt.return % subtitle) String.Sensible.compare;
    increasing (Lwt.return % subtitle) String.compare_lengths;
  ]

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Book.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
