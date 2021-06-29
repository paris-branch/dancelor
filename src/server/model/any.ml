open Nes

include Dancelor_common_model.Any

module Filter = struct
  include Filter

  let accepts filter any =
    Formula.interpret filter @@ function
    | Is any' -> equal any any'
    | TypeIs type_ -> Lwt.return (Type.equal (type_of any) type_)
end

module E = Dancelor_common_model.Any_endpoints
module A = E.Arguments

let book_search ?pagination ?threshold input =
  Book.search ?pagination ?threshold input

let set_search ?pagination ?threshold input =
  Set.search ?pagination ?threshold input

let tune_search ?pagination ?threshold input =
  Tune.search ?pagination ?threshold input

let version_search ?pagination ?threshold input =
  Version.search ?pagination ?threshold input

let apply_filter_on_scores filter all =
  Score.list_filter_lwt (Filter.accepts filter) all

let search ?filter ?pagination ?threshold input =
  let search_wrap_and_add search wrapper list =
    let%lwt scores = search ?pagination ?threshold input in
    let scores = Score.list_map wrapper scores in
    Lwt.return (list @ scores)
  in
  Lwt.return []
  >>=| search_wrap_and_add Credit.search (fun c -> Credit c)
  >>=| search_wrap_and_add Dance.search (fun c -> Dance c)
  >>=| search_wrap_and_add Person.search (fun c -> Person c)
  >>=| search_wrap_and_add book_search (fun c -> Book c)
  >>=| search_wrap_and_add set_search (fun c -> Set c)
  >>=| search_wrap_and_add Source.search (fun c -> Source c)
  >>=| search_wrap_and_add tune_search (fun c -> Tune c)
  >>=| search_wrap_and_add version_search (fun c -> Version c)
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter_on_scores filter
  >>=| Score.list_sort_decreasing
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?filter:(o A.filter)
      ?pagination:(o A.pagination)
      ?threshold:(o A.threshold)
      (a A.string)
  )
