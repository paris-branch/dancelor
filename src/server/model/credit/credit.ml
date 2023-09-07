open Nes
include CreditLifted

let make_and_save ?status ~line ?persons ?scddb_id ~modified_at ~created_at () =
  let%lwt persons =
    let%olwt persons = Lwt.return persons in
    let%lwt persons = Lwt_list.map_s Person.slug persons in
    Lwt.return_some persons
  in
  Dancelor_server_database.Credit.save ~slug_hint:line @@ fun slug ->
  Lwt.return (make ?status ~slug ~line ?persons ~scddb_id ~modified_at ~created_at ()) (* FIXME: status should probably go in save *)

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~line:   (a A.line)
      ?persons:(o A.persons)
      ?scddb_id:(o A.scddb_id)
      ~modified_at:(a A.modified_at)
      ~created_at:(a A.created_at)
      ()
  )

let search ?pagination ?(threshold=Float.min_float) filter =
  Dancelor_server_database.Credit.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing line String.Sensible.compare
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
