open Nes
include SetCore

module E = Dancelor_common_model.Set_endpoints
module A = E.Arguments

let make_and_save ?status ~name ?deviser ~kind ?versions_and_parameters ?dances () =
  Dancelor_server_database.Set.save ~slug_hint:name @@ fun slug ->
  make ?status ~slug ~name ?deviser ~kind ?versions_and_parameters ?dances ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~name:   (a A.name)
      ?deviser:(o A.deviser)
      ~kind:   (a A.kind)
      ?status: (o A.status)
      ?versions_and_parameters:(o A.versions_and_parameters)
      ?dances:(o A.dances)
      ()
  )

let delete s =
  let%lwt slug = slug s in
  Dancelor_server_database.Set.delete slug

let () =
  Madge_server.(
    register ~endpoint:E.delete @@ fun {a} _ ->
    let%lwt set = get (a A.slug) in
    delete set
  )

let search ?pagination ?(threshold=Float.min_float) filter =
  Dancelor_server_database.Set.get_all ()
  >>=| Score.lwt_map_from_list (SetFilter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare;
      increasing name String.compare_lengths;
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

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
