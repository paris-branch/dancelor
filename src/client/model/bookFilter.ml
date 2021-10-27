open Nes
open BookCore
include Dancelor_common_model.BookFilter

let accepts filter book =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Is book' ->
    equal book book' >|=| Formula.interpret_bool

  | Title string ->
    let%lwt title = BookCore.title book in
    Lwt.return (String.proximity ~char_equal string title)

  | TitleMatches string ->
    let%lwt title = BookCore.title book in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string title)

  | Subtitle string ->
    let%lwt subtitle = BookCore.subtitle book in
    Lwt.return (String.proximity ~char_equal string subtitle)

  | SubtitleMatches string ->
    let%lwt subtitle = BookCore.subtitle book in
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
    Formula.interpret_exists (VersionFilter.accepts vfilter) versions

  | ExistsSet sfilter ->
    let%lwt content = contents book in
    let%lwt sets =
      Lwt_list.filter_map_s
        (function
          | Set (s, _p) -> Lwt.return_some s
          | _ -> Lwt.return_none)
        content
    in
    Formula.interpret_exists (SetFilter.accepts sfilter) sets

  | ExistsInlineSet sfilter ->
    let%lwt content = contents book in
    let%lwt isets =
      Lwt_list.filter_map_s
        (function
          | InlineSet (s, _p) -> Lwt.return_some s
          | _ -> Lwt.return_none)
        content
    in
    Formula.interpret_exists (SetFilter.accepts sfilter) isets
