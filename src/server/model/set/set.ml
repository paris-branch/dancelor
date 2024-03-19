open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include SetLifted

module E = Common.Model.SetEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?conceptors ~kind ?contents
    ~order ?dances ~modified_at ~created_at
    ()
  =
  Database.Set.save ~slug_hint:name @@ fun slug ->
  make
    ?status ~slug ~name ?conceptors ~kind ?contents
    ~order ?dances ~modified_at ~created_at ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~name:   (a A.name)
      ?conceptors: (o A.conceptors)
      ~kind:   (a A.kind)
      ?status: (o A.status)
      ?contents: (o A.contents)
      ~order:  (a A.order)
      ?dances: (o A.dances)
      ~modified_at: (a A.modified_at)
      ~created_at:  (a A.created_at)
      ()
  )

let delete = Database.Set.delete % slug

let () =
  Madge_server.(
    register ~endpoint:E.delete @@ fun {a} _ ->
    let%lwt set = get (a A.slug) in
    delete set
  )

let tiebreakers = Lwt_list.[
    increasing (Lwt.return % name) String.Sensible.compare;
    increasing (Lwt.return % name) String.compare_lengths;
  ]

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Set.get_all
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
