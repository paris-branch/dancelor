open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include DanceLifted

module E = Common.Model.DanceEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ~kind ?deviser ~two_chords ?scddb_id
    ?disambiguation ~modified_at ~created_at
    ()
  =
  Database.Dance.save ~slug_hint:name @@ fun slug ->
  make
    ?status ~slug ~name ~kind ?deviser ~two_chords ?scddb_id
    ?disambiguation ~modified_at ~created_at
    ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ?status:      (o A.status)
      ~name:        (a A.name)
      ~kind:        (a A.kind)
      ?deviser:     (o A.deviser)
      ~two_chords:  (a A.two_chords)
      ?scddb_id:    (o A.scddb_id)
      ~modified_at: (a A.modified_at)
      ~created_at:  (a A.created_at)
      ()
  )

let search ?pagination ?(threshold=Float.min_float) filter =
  let module Score = Common.Model.Score in
  let%lwt results =
    Database.Dance.get_all ()
    >>=| Score.lwt_map_from_list (Filter.accepts filter)
    >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
    >>=| Score.(list_proj_sort_decreasing [
        increasing (Lwt.return % name) String.Sensible.compare
      ])
  in
  Lwt.return @@ Option.fold ~none:Fun.id ~some:Common.Model.Pagination.apply pagination results

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )
