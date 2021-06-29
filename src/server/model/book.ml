open Nes
module E = Dancelor_common_model.Book_endpoints
module A = E.Arguments

include Dancelor_common_model.Book

let contents p =
  let%lwt contents = contents p in
  Lwt_list.map_p
    (function
      | (Version (v, p) : page_slug) -> let%lwt v = Version.get v in Lwt.return (Version (v, p))
      | Set (s, p) -> let%lwt s = Set.get s in Lwt.return (Set (s, p))
      | InlineSet (s, p) -> Lwt.return (InlineSet (s, p)))
    contents

let warnings _p = assert false (* FIXME *)

module Filter = struct
  include Filter

  let accepts filter book =
    match filter with

    | Is book' ->
      equal book book'

    | ExistsVersion vfilter ->
      let%lwt content = contents book in
      let%lwt versions =
        Lwt_list.filter_map_s
          (function
            | Version (v, _p) -> Lwt.return_some v
            | _ -> Lwt.return_none)
          content
      in
      Lwt_list.exists_s (Version.Filter.accepts vfilter) versions

    | ExistsSet sfilter ->
      let%lwt content = contents book in
      let%lwt sets =
        Lwt_list.filter_map_s
          (function
            | Set (s, _p) -> Lwt.return_some s
            | _ -> Lwt.return_none)
          content
      in
      Lwt_list.exists_s (Set.Filter.accepts sfilter) sets
end

let get = Dancelor_server_database.Book.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let apply_filter filter all =
  Lwt_list.filter_s (Formula.accepts Filter.accepts filter) all

let apply_filter_on_scores filter all =
  Score.list_filter_lwt (Formula.accepts Filter.accepts filter) all

let all ?filter ?pagination () =
  Dancelor_server_database.Book.get_all ()
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter filter
  >>=| Lwt_list.(sort_multiple [
      increasing     date NesDate.compare ;
      increasing    title String.Sensible.compare ;
      increasing subtitle String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.all @@ fun _ {o} ->
    all
      ?filter:(o A.filter)
      ?pagination:(o A.pagination)
      ()
  )

let search string book =
  let%lwt title = title book in
  String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle:string title
  |> Lwt.return

let search ?filter ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Book.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter_on_scores filter
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing     date NesDate.compare ;
      increasing    title String.Sensible.compare ;
      increasing subtitle String.Sensible.compare
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
