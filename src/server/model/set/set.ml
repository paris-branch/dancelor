open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include SetLifted

module E = Common.Model.SetEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?deviser ~kind ?versions_and_parameters
    ~order ?dances ~modified_at ~created_at
    ()
  =
  Database.Set.save ~slug_hint:name @@ fun slug ->
  make
    ?status ~slug ~name ?deviser ~kind ?versions_and_parameters
    ~order ?dances ~modified_at ~created_at ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~name:   (a A.name)
      ?deviser:(o A.deviser)
      ~kind:   (a A.kind)
      ?status: (o A.status)
      ?versions_and_parameters:(o A.versions_and_parameters)
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

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Set.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers: Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare;
        increasing (Lwt.return % name) String.compare_lengths;
      ]

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let search' ?pagination ?threshold filter =
  Lwt.map snd @@ search ?pagination ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
