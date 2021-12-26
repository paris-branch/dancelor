open Nes
include BookCore

module E = Dancelor_common_model.BookEndpoints
module A = E.Arguments

let search ?pagination ?(threshold=Float.min_float) filter =
  Dancelor_server_database.Book.get_all ()
  >>=| Score.lwt_map_from_list (BookFilter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing     date NesDate.compare ;
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
