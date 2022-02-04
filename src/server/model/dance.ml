open Nes
include DanceLifted

module E = Dancelor_common_model.DanceEndpoints
module A = E.Arguments

let search ?pagination ?(threshold=Float.min_float) filter =
  Dancelor_server_database.Dance.get_all ()
  >>=| Score.lwt_map_from_list (DanceFilter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare
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
