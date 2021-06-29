open Nes

include Dancelor_common_model.Tune

let author = author >=>?| (Credit.get >=>| Lwt.return_some)
let dances = dances >=>| Lwt_list.map_p Dance.get

module Filter = struct
  include Filter

  let accepts filter tune =
    match filter with
    | Is tune' ->
      let%lwt slug' = slug tune' in
      let%lwt slug  = slug tune  in
      Lwt.return (Slug.equal slug slug')
    | Author afilter ->
      (match%lwt author tune with
       | None -> Lwt.return_true
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

let search string person =
  let%lwt name = name person in
  String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle:string name
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Tune.get_all ()
  >>=| Score.lwt_map_from_list (search string)
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
      (a A.string)
  )
