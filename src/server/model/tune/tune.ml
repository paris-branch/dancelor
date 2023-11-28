open Nes
include TuneLifted

module E = Dancelor_common_model.TuneEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?alternative_names ~kind ?author
    ?dances ?remark ?scddb_id ~modified_at ~created_at
    ()
  =
  Dancelor_server_database.Tune.save ~slug_hint:name @@ fun slug ->
  make
    ?status ~slug ~name ?alternative_names ~kind ?author
    ?dances ?remark ?scddb_id ~modified_at ~created_at
    ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ?status:  (o A.status)
      ~name:    (a A.name)
      ?alternative_names:(o A.alternative_names)
      ~kind:    (a A.kind)
      ?author:  (o A.author)
      ?dances:  (o A.dances)
      ?remark:  (o A.remark)
      ?scddb_id:(o A.scddb_id)
      ~modified_at:(a A.modified_at)
      ~created_at:(a A.created_at)
      ()
  )

let search ?pagination ?(threshold=Float.min_float) filter =
  Dancelor_server_database.Tune.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare;
      increasing name String.compare_lengths;
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
