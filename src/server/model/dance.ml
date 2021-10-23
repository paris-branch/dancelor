open Nes
module E = Dancelor_common_model.Dance_endpoints
module A = E.Arguments

include Dancelor_common_model.Dance

let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)

let get = Dancelor_server_database.Dance.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let search ?pagination ?(threshold=0.) filter =
  Dancelor_server_database.Dance.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
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
