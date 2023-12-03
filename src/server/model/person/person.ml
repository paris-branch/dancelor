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

let search ?pagination ?(threshold=Float.min_float) filter =
  let module Score = Common.Model.Score in
  let%lwt results =
    Database.Person.get_all ()
    >>=| Score.lwt_map_from_list (Filter.accepts filter)
    >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
    >>=| Score.(list_proj_sort_decreasing [
        increasing (Lwt.return % name) String.Sensible.compare
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
