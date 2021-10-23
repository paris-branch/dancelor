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

  let warnings p =
    let warnings = ref [] in
    let add_warning w = warnings := w :: !warnings in

    let%lwt contents = contents p in

    (* Check that there are no duplicate sets. *)
    let%lwt sets =
      Lwt_list.filter_map_p
        (function
          | Version _ -> Lwt.return_none
          | Set (s, _) | InlineSet (s, _) -> Lwt.return_some s)
        contents
    in
    let sets = List.sort Stdlib.compare sets in
    (match sets with
     | [] -> add_warning Empty
     | set :: sets ->
       let _ =
         List.fold_left
           (fun prev curr ->
              if prev = curr then
                add_warning (DuplicateSet curr);
              curr)
           set sets
       in
       ());

    (* Check that there are no duplicate tune. *)
    let%lwt standalone_versions =
      Lwt_list.filter_map_p
        (function
          | Version (v, _) -> Lwt.return_some v
          | _ -> Lwt.return_none)
        contents
    in
    let%lwt contained_versions =
      let%lwt versions_and_parameters = Lwt_list.map_s Set.versions_and_parameters sets in
      let versions_and_parameters = List.flatten versions_and_parameters in
      Lwt.return (List.map fst versions_and_parameters)
    in
    let versions = standalone_versions @ contained_versions in
    let%lwt tunes = Lwt_list.map_s Version.tune versions in
    let tunes = List.sort Stdlib.compare tunes in
    (match tunes with
     | [] -> ()
     | tune :: tunes ->
       let _ =
         List.fold_left
           (fun prev curr ->
              if prev = curr then
                add_warning (DuplicateVersion curr);
              curr)
           tune
           tunes
       in
       ());

    (* Return *)
    Lwt.return !warnings
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

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter;
  )
