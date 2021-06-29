open Nes

include Dancelor_common_model.Tune

let author = author >=>?| (Credit.get >=>| Lwt.return_some)
let dances = dances >=>| Lwt_list.map_p Dance.get

module Filter = struct
  include Filter

  let accepts filter tune =
    match filter with

    | Is tune' ->
      equal tune tune'

    | Author afilter ->
      (match%lwt author tune with
       | None -> Lwt.return_false
       | Some author -> Credit.Filter.accepts afilter author)

    | AuthorIsDefined ->
      let%lwt author = author tune in
      Lwt.return (author <> None)

    | Kind kind' ->
      let%lwt kind = kind tune in
      Lwt.return (kind = kind')
end

module E = Dancelor_common_model.Tune_endpoints
module A = E.Arguments

let get = Dancelor_server_database.Tune.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let apply_filter filter all =
  Lwt_list.filter_s (Filter.accepts filter) all

let apply_filter_on_scores filter all =
  Score.list_filter_lwt (Filter.accepts filter) all

let all ?filter ?pagination () =
  Dancelor_server_database.Tune.get_all ()
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter filter
  >>=| Lwt_list.(sort_multiple [
      increasing name String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.all @@ fun _ {o} ->
    all ?filter:(o A.filter) ?pagination:(o A.pagination) ()
  )

let search string person =
  let%lwt name = name person in
  String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle:string name
  |> Lwt.return

let search ?filter ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Tune.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter_on_scores filter
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?filter:    (o A.filter)
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.string)
  )
