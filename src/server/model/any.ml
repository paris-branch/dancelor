open Nes
include Dancelor_common_model.Any

let version_search ?pagination ?threshold input =
  Version.search ?pagination ?threshold input

let search ?pagination ?threshold ?(except=[]) input =
  let search_wrap_and_add search wrapper list =
    let%lwt scores = search ?pagination ?threshold input in
    let scores = Score.list_map wrapper scores in
    Lwt.return (list @ scores)
  in
  let%lwt scores =
    Lwt.return []
    >>=| search_wrap_and_add Credit.search (fun c -> Credit c)
    >>=| search_wrap_and_add Dance.search (fun c -> Dance c)
    >>=| search_wrap_and_add Person.search (fun c -> Person c)
    >>=| search_wrap_and_add Book.search (fun c -> Book c)
    >>=| search_wrap_and_add Set.search (fun c -> Set c)
    >>=| search_wrap_and_add Source.search (fun c -> Source c)
    >>=| search_wrap_and_add Tune.search (fun c -> Tune c)
    >>=| search_wrap_and_add version_search (fun c -> Version c)
  in
  let%lwt scores =
    scores
    |> Score.list_filter (fun value -> List.for_all ((<>) (type_of value)) except)
    |> Score.list_sort_decreasing
  in
  Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination scores

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      ?except: (o Arg.type_)
      (a Arg.string)
  )
