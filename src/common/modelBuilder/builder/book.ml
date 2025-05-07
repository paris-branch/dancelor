open Nes

module Build
  (Dance : Signature.Dance.S)
  (Person : Signature.Person.S)
  (Set : Signature.Set.S)
  (Tune : Signature.Tune.S)
  (Version : Signature.Version.S)
= struct
  include Core.Book

  let short_title book = if short_title book = "" then title book else short_title book
  let short_title' = short_title % Entry.value

  let authors = Lwt_list.map_p Person.get % authors
  let authors' = authors % Entry.value

  let compare : t Entry.t -> t Entry.t -> int =
    Slug.compare_slugs_or
      ~fallback: (fun book1 book2 ->
        (* Compare first by date *)
        let c = compare (Entry.value book1).date (Entry.value book2).date in
        if c = 0 then
          compare book1 book2
        else
          c
      )
      Entry.slug'

  let equal book1 book2 = compare book1 book2 = 0

  let contents book =
    Lwt_list.map_p
      (function
        | Core.Book.Page.Version (version, parameters) ->
          let%lwt version = Version.get version in
          Lwt.return (Version (version, parameters))
        | Core.Book.Page.Set (set, parameters) ->
          let%lwt set = Set.get set in
          Lwt.return (Set (set, parameters))
        | Core.Book.Page.InlineSet (set, parameters) ->
          Lwt.return (InlineSet (set, parameters))
      )
      (contents book)

  let contents' = contents % Entry.value

  let versions_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (function
        | Version (version, _) -> Lwt.return_some version
        | _ -> Lwt.return_none
      )
      contents

  let versions_from_contents' = versions_from_contents % Entry.value

  let isInlineSet = function
    | InlineSet _ -> true
    | _ -> false

  let find_context_no_inline index set =
    let%lwt contents = contents set in
    let contents_no_inline = List.filter (not % isInlineSet) contents in
    Lwt.return @@ List.findi_context (fun i _ -> i = index) contents_no_inline

  let find_context_no_inline' index = find_context_no_inline index % Entry.value

  let lilypond_contents_cache_key book =
    let%lwt pages = contents book in
    let%lwt contents =
      Lwt_list.map_p
        (function
          | Version (version, _) -> Lwt.return @@ Version.content' version
          | Set (set, _) -> Set.lilypond_content_cache_key' set
          | InlineSet (set, _) -> Set.lilypond_content_cache_key set
        )
        pages
    in
    Lwt.return (String.concat "\n" contents)

  let lilypond_contents_cache_key' = lilypond_contents_cache_key % Entry.value

  let page_to_page_core = function
    | (Version (version, params): page) -> Core.Book.Page.Version (Entry.slug version, params)
    | (Set (set, params): page) -> Core.Book.Page.Set (Entry.slug set, params)
    | (InlineSet (set, params): page) -> Core.Book.Page.InlineSet (set, params)

  let make ~title ?subtitle ?short_title ?authors ?date ?contents ?source ?remark ?scddb_id () =
    let title = String.remove_duplicates ~char: ' ' title in
    let subtitle = Option.map (String.remove_duplicates ~char: ' ') subtitle in
    let short_title = Option.map (String.remove_duplicates ~char: ' ') short_title in
    let authors = Option.map (List.map Entry.slug) authors in
    let contents = Option.map (List.map page_to_page_core) contents in
    make ~title ?subtitle ?short_title ?authors ?date ?contents ?source ?remark ?scddb_id ()

  module Warnings = struct
    (* The following functions all have the name of a warning of
       {!Dancelor_common.Model.Core.Book.warning}. They all are in charge of
       generating a list of the associated warning corresponding to the given
       book. The {!all} function then gathers all these warnings in a common list. *)

    let empty book =
      let%lwt contents = contents' book in
      if contents = [] then
        Lwt.return [Empty]
      else
        Lwt.return_nil

    let sets_from_contents' book =
      let%lwt contents = contents' book in
      Lwt_list.filter_map_p
        (function
          | Version _ | InlineSet _ -> Lwt.return_none
          | Set (set, _) -> Lwt.return_some set
        )
        contents

    let duplicateSet book =
      Fun.flip Lwt.map (sets_from_contents' book) @@ fun sets ->
      match List.sort Set.compare sets with
      | [] -> []
      | first_set :: other_sets ->
        let (_, warnings) =
          List.fold_left
            (fun (previous_set, warnings) current_set ->
              let warnings =
                if Set.equal current_set previous_set then
                    ((DuplicateSet current_set) :: warnings)
                else
                  warnings
              in
                (current_set, warnings)
            )
            (first_set, [])
            other_sets
        in
        warnings

    let unique_sets_from_contents' =
      Lwt.map (List.sort_uniq Set.compare) % sets_from_contents'

    let duplicateVersion book =
      let%lwt sets = unique_sets_from_contents' book in
      let%lwt standalone_versions = versions_from_contents' book in
      (* [tunes_to_sets] is a hashtable from tunes to sets they belong to.
         Standalone tunes are associated with None *)
      let tunes_to_sets = Hashtbl.create 8 in
      (* Extend the list of sets associated to this tune. Creates it if it was not
         yet in the hashtable *)
      let register_tune_to_set tune set_opt =
        match Hashtbl.find_opt tunes_to_sets tune with
        | None -> Hashtbl.add tunes_to_sets tune [set_opt]
        | Some set_opts -> Hashtbl.replace tunes_to_sets tune (set_opt :: set_opts)
      in
      (* register standalone tunes *)
      Lwt_list.iter_s
        (fun v ->
          let%lwt tune = Version.tune' v in
          register_tune_to_set tune None;
          Lwt.return ()
        )
        standalone_versions;%lwt
      (* register tunes in sets *)
      Lwt_list.iter_s
        (fun set ->
          let%lwt contents = Set.contents' set in
          let versions = List.map fst contents in
          Lwt_list.iter_s
            (fun v ->
              let%lwt tune = Version.tune' v in
              register_tune_to_set tune (Some set);
              Lwt.return ()
            )
            versions
        )
        sets;%lwt
      (* crawl all registered tunes and see if they appear several times. if that is
         the case, add a warning accordingly *)
      Hashtbl.to_seq tunes_to_sets
      |> List.of_seq
      |> List.fold_left
          (fun warnings (tune, set_opts) ->
            let set_opts = List.sort_count (Option.compare Set.compare) set_opts in
            if List.length set_opts > 1 then
                ((DuplicateVersion (tune, set_opts)) :: warnings)
            else
              warnings
          )
          []
      |> Lwt.return

    let sets_and_parameters_from_contents' book =
      let%lwt contents = contents' book in
      Lwt_list.filter_map_p
        (function
          | Version _ | InlineSet _ -> Lwt.return_none
          | Set (set, parameters) -> Lwt.return_some (set, parameters)
        )
        contents

    let setDanceMismatch book =
      let%lwt sets_and_parameters = sets_and_parameters_from_contents' book in
      Lwt_list.filter_map_p
        (fun (set, parameters) ->
          let%olwt dance_slug = Lwt.return (Core.SetParameters.for_dance parameters) in
          (* FIXME: SetParameters should be hidden behind the same kind of
             mechanism as the rest; and this step should not be necessary *)
          let%lwt dance = Dance.get dance_slug in
          if Set.kind' set = Dance.kind' dance then
            Lwt.return_none
          else
            Lwt.return_some (SetDanceMismatch (set, dance))
        )
        sets_and_parameters

    let all book =
      Lwt_list.fold_left_s
        (fun warnings new_warnings_lwt ->
          let%lwt new_warnings = new_warnings_lwt in
          Lwt.return (warnings @ new_warnings)
        )
        []
        [
          empty book;
          duplicateSet book;
          duplicateVersion book;
          setDanceMismatch book
        ]
  end

  let warnings book = Warnings.all book

  let page_core_to_page = function
    | Core.Book.Page.Version (version, params) ->
      let%lwt version = Version.get version in
      Lwt.return @@ Version (version, params)
    | Core.Book.Page.Set (set, params) ->
      let%lwt set = Set.get set in
      Lwt.return @@ Set (set, params)
    | Core.Book.Page.InlineSet (set, params) ->
      Lwt.return @@ InlineSet (set, params)

  module Filter = struct
    include Filter.Book

    let rec accepts filter book =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
        | Is book' ->
          Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug book) book'
        | Title string ->
          Lwt.return @@ String.proximity ~char_equal string @@ Core.Book.title' book
        | TitleMatches string ->
          Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Core.Book.title' book
        | Subtitle string ->
          Lwt.return @@ String.proximity ~char_equal string @@ Core.Book.subtitle' book
        | SubtitleMatches string ->
          Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Core.Book.subtitle' book
        | IsSource ->
          Lwt.return @@ Formula.interpret_bool @@ source' book
        | ExistsVersion vfilter ->
          let%lwt content = contents' book in
          let%lwt versions =
            Lwt_list.filter_map_s
              (function
                | Version (v, _p) -> Lwt.return_some v
                | _ -> Lwt.return_none
              )
              content
          in
          Formula.interpret_exists (Version.Filter.accepts vfilter) versions
        | ExistsSet sfilter ->
          let%lwt content = contents' book in
          let%lwt sets =
            Lwt_list.filter_map_s
              (function
                | Set (s, _p) -> Lwt.return_some s
                | _ -> Lwt.return_none
              )
              content
          in
          Formula.interpret_exists (Set.Filter.accepts sfilter) sets
        | ExistsInlineSet sfilter ->
          let%lwt content = contents' book in
          let%lwt isets =
            Lwt_list.filter_map_s
              (function
                | InlineSet (s, _p) -> Lwt.return_some s
                | _ -> Lwt.return_none
              )
              content
          in
          Formula.interpret_exists (Set.Filter.accepts sfilter % Entry.make_dummy) isets
        | ExistsVersionDeep vfilter ->
          (* recursive call to check the compound formula *)
          Fun.flip accepts book @@
            Formula.or_l
              [
                Formula.pred (ExistsVersion vfilter);
                Formula.pred (ExistsSet (Set.Filter.existsVersion' vfilter));
                Formula.pred (ExistsInlineSet (Set.Filter.existsVersion' vfilter));
              ]
  end
end
