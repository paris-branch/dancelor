open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include BookLifted

let make_and_save
    ?status
    ~title
    ?date
    ?contents
    ~modified_at
    ~created_at
    ()
  =
  Database.Book.save ~slug_hint: title @@ fun slug ->
  make ?status ~slug ~title ?date ?contents ~modified_at ~created_at ()

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

let update
    ?status
    ~slug
    ~title
    ?date
    ?contents
    ~modified_at
    ~created_at
    ()
  =
  let%lwt book = make ?status ~slug ~title ?date ?contents ~modified_at ~created_at () in
  Database.Book.update book
