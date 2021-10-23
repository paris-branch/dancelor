open Nes
module E = Dancelor_common_model.Book_endpoints
module A = E.Arguments

module Self = struct
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
end
include Self

module Filter = struct
  include Filter

  let accepts filter book =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is book' ->
      equal book book' >|=| Formula.interpret_bool

    | Title string ->
      let%lwt title = Self.title book in
      Lwt.return (String.proximity ~char_equal string title)

    | TitleMatches string ->
      let%lwt title = Self.title book in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string title)

    | Subtitle string ->
      let%lwt subtitle = Self.subtitle book in
      Lwt.return (String.proximity ~char_equal string subtitle)

    | SubtitleMatches string ->
      let%lwt subtitle = Self.subtitle book in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string subtitle)

    | IsSource ->
      is_source book >|=| Formula.interpret_bool

    | ExistsVersion vfilter ->
      let%lwt content = contents book in
      let%lwt versions =
        Lwt_list.filter_map_s
          (function
            | Version (v, _p) -> Lwt.return_some v
            | _ -> Lwt.return_none)
          content
      in
      Formula.interpret_exists (Version.Filter.accepts vfilter) versions

    | ExistsSet sfilter ->
      let%lwt content = contents book in
      let%lwt sets =
        Lwt_list.filter_map_s
          (function
            | Set (s, _p) -> Lwt.return_some s
            | _ -> Lwt.return_none)
          content
      in
      Formula.interpret_exists (Set.Filter.accepts sfilter) sets

    | ExistsInlineSet sfilter ->
      let%lwt content = contents book in
      let%lwt isets =
        Lwt_list.filter_map_s
          (function
            | InlineSet (s, _p) -> Lwt.return_some s
            | _ -> Lwt.return_none)
          content
      in
      Formula.interpret_exists (Set.Filter.accepts sfilter) isets
end

let get = Dancelor_server_database.Book.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let search ?pagination ?(threshold=0.) filter =
  Dancelor_server_database.Book.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
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
