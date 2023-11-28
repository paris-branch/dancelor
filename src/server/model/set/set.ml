open Nes
include SetLifted

module E = Dancelor_common_model.SetEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?deviser ~kind ?versions_and_parameters
    ~order ?dances ~modified_at ~created_at
    ()
  =
  Dancelor_server_database.Set.save ~slug_hint:name @@ fun slug ->
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

let delete = Dancelor_server_database.Set.delete % slug

let () =
  Madge_server.(
    register ~endpoint:E.delete @@ fun {a} _ ->
    let%lwt set = get (a A.slug) in
    delete set
  )

let search ?pagination ?(threshold=Float.min_float) filter =
  Dancelor_server_database.Set.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing (Lwt.return % name) String.Sensible.compare;
      increasing (Lwt.return % name) String.compare_lengths;
    ])
  >>=| Option.fold ~none:Lwt.return ~some:Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let count filter =
  let%lwt l = search filter in
  Lwt.return (List.length l)

let () =
  Madge_server.register ~endpoint:E.count @@ fun {a} _ ->
  count (a A.filter)
