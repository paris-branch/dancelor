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

let search ?pagination ?(threshold=Float.min_float) filter =
  let module Score = Common.Model.Score in
  let%lwt results =
    Database.Set.get_all ()
    >>=| Score.lwt_map_from_list (Filter.accepts filter)
    >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
    >>=| Score.(list_proj_sort_decreasing [
        increasing (Lwt.return % name) String.Sensible.compare;
        increasing (Lwt.return % name) String.compare_lengths;
      ])
  in
  Lwt.return (
    List.length results,
    Option.fold ~none:Fun.id ~some:Common.Model.Pagination.apply pagination results
  )

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
