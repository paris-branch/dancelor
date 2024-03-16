open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include PersonLifted

let make_and_save ?status ~name ?scddb_id ~modified_at ~created_at () =
  Database.Person.save ~slug_hint:name @@ fun slug ->
  Lwt.return (make ?status ~slug ~name ~scddb_id ~modified_at ~created_at ()) (* FIXME: status should probably go in save *)

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~name:   (a A.name)
      ?scddb_id:(o A.scddb_id)
      ~modified_at:(a A.modified_at)
      ~created_at:(a A.created_at)
      ()
  )

let tiebreakers = Lwt_list.[
    increasing (Lwt.return % name) String.Sensible.compare
  ]

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Person.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?slice: (o A.slice)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
