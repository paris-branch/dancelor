open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include TuneLifted

module E = Common.Model.TuneEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?alternative_names ~kind ?composer
    ?dances ?remark ?scddb_id ?date ~modified_at ~created_at
    ()
  =
  Database.Tune.save ~slug_hint:name @@ fun slug ->
  make
    ?status ~slug ~name ?alternative_names ~kind ?composer
    ?dances ?remark ?scddb_id ?date ~modified_at ~created_at
    ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ?status:  (o A.status)
      ~name:    (a A.name)
      ?alternative_names:(o A.alternative_names)
      ~kind:    (a A.kind)
      ?composer:  (o A.composer)
      ?dances:  (o A.dances)
      ?remark:  (o A.remark)
      ?scddb_id:(o A.scddb_id)
      ?date: (o A.date)
      ~modified_at:(a A.modified_at)
      ~created_at:(a A.created_at)
      ()
  )

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Tune.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers: Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare;
        increasing (Lwt.return % name) String.compare_lengths;
      ]

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
