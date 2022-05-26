open Nes
include BookLifted

module E = Dancelor_common_model.BookEndpoints
module A = E.Arguments

let make_and_save
    ?status ~title ?date ?contents_and_parameters ()
  =
  Dancelor_server_database.Book.save ~slug_hint:title @@ fun slug ->
  make ?status ~slug ~title ?date ?contents_and_parameters ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ?status:    (o A.status)
      ~title:     (a A.title)
      ?date:      (o A.date)
      ?contents_and_parameters:(o A.contents_and_parameters)
      ()
  )

let search ?pagination ?(threshold=Float.min_float) filter =
  Dancelor_server_database.Book.get_all ()
  >>=| Score.lwt_map_from_list (BookFilter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      decreasing     date NesDate.compare ;
      increasing    title String.Sensible.compare ;
      increasing    title String.compare_lengths ;
      increasing subtitle String.Sensible.compare ;
      increasing subtitle String.compare_lengths ;
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
