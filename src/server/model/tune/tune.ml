open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include TuneLifted

module E = Common.Model.TuneEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?alternative_names ~kind ?author
    ?dances ?remark ?scddb_id ~modified_at ~created_at
    ()
  =
  Database.Tune.save ~slug_hint:name @@ fun slug ->
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

(* Cache for search requests with a lifetime of 10 minutes. *)
let cache = Cache.create ~lifetime:600 ()

let search ?pagination ?(threshold=Float.min_float) filter =
  let module Score = Common.Model.Score in
  let%lwt results =
    Cache.use ~cache ~key:(threshold, filter) @@ fun () ->
    Database.Tune.get_all ()
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
