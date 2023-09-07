open Nes

module Lift
    (Dance: module type of DanceSignature)
    (Set: module type of SetSignature)
    (Tune: module type of TuneSignature)
    (Version: module type of VersionSignature)
= struct
  include BookCore

  let title book = Lwt.return book.title
  let subtitle book = Lwt.return book.subtitle
  let short_title book = if book.short_title = "" then title book else Lwt.return book.short_title
  let date book = Lwt.return book.date
  let contents book = Lwt.return book.contents
  let source book = Lwt.return book.source (* FIXME: Should be removed *)
  let remark book = Lwt.return book.remark
  let scddb_id book = Lwt.return book.scddb_id
  let modified_at book = Lwt.return book.modified_at
  let created_at book = Lwt.return book.created_at

  let equal book1 book2 =
    let%lwt slug1 = slug book1 in
    let%lwt slug2 = slug book2 in
    Lwt.return (Slug.equal slug1 slug2)

  let is_source book = source book

  let compare book1 book2 =
    (* Compare first by date *)
    let c = compare book1.date book2.date in
    if c = 0 then
      compare book1 book2
    else
      c

  let contents book =
    let%lwt contents = contents book in
    Lwt_list.map_p
      (
        function
        | PageCore.Version (version, parameters) ->
          let%lwt version = Version.get version in
          Lwt.return (Version (version, parameters))
        | PageCore.Set (set, parameters) ->
          let%lwt set = Set.get set in
          Lwt.return (Set (set, parameters))
        | PageCore.InlineSet (set, parameters) ->
          Lwt.return (InlineSet (set, parameters))
      )
      contents

  let versions_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (
        function
        | Version (version, _) -> Lwt.return_some version
        | _ -> Lwt.return_none
      )
      contents

  let sets_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (
        function
        | Version _ -> Lwt.return_none
        | Set (set, _) | InlineSet (set, _) -> Lwt.return_some set
      )
      contents

  let unique_sets_from_contents book =
    let%lwt sets = sets_from_contents book in
    List.sort_uniq_lwt Set.compare sets

  let sets_and_parameters_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (
        function
        | Set (set, parameters) | InlineSet (set, parameters) ->
          Lwt.return_some (set, parameters)
        | Version _ -> Lwt.return_none
      )
      contents

  let lilypond_contents_cache_key book =
    let%lwt pages = contents book in
    let%lwt contents =
      Lwt_list.map_p
        (
          function
          | Version (version, _) -> Version.content version
          | Set (set, _) | InlineSet (set, _) -> Set.lilypond_content_cache_key set
        )
        pages
    in
    Lwt.return (String.concat "\n" contents)

  let page_to_page_core = function
    | (Version (version, params): page) ->
      let%lwt slug = Version.slug version in
      Lwt.return @@ PageCore.Version (slug, params)
    | (Set (set, params): page) ->
      let%lwt slug = Set.slug set in
      Lwt.return @@ PageCore.Set (slug, params)
    | (InlineSet (set, params): page) ->
      Lwt.return @@ PageCore.InlineSet (set, params)

  let make ?status ~slug ~title ?date ?contents_and_parameters ~modified_at ~created_at () =
    let%lwt contents_and_parameters =
      let%olwt contents = Lwt.return contents_and_parameters in
      let%lwt contents = Lwt_list.map_s page_to_page_core contents in
      Lwt.return_some contents
    in
    Lwt.return
      (
        make
          ?status
          ~slug
          ~title
          ?date
          ?contents: contents_and_parameters
          ~modified_at
          ~created_at
          ()
      )

  module Warnings = struct
    (* The following functions all have the name of a warning of
       {!Dancelor_common_model.BookCore.warning}. They all are in charge of
       generating a list of the associated warning corresponding to the given
       book. The {!all} function then gathers all these warnings in a common list. *)

    let empty book =
      let%lwt contents = contents book in
      if contents = [] then
        Lwt.return [Empty]
      else
        Lwt.return_nil

    let duplicateSet book =
      let%lwt sets = sets_from_contents book in
      match%lwt List.sort_lwt Set.compare sets with
      | [] -> Lwt.return_nil
      | first_set :: other_sets ->
        let%lwt (_, warnings) =
          Lwt_list.fold_left_s
            (
              fun (previous_set, warnings) current_set ->
                let%lwt warnings =
                  if%lwt Set.equal current_set previous_set then
                    Lwt.return ((DuplicateSet current_set) :: warnings)
                  else
                    Lwt.return warnings
                in
                Lwt.return (current_set, warnings)
            )
            (first_set, [])
            other_sets
        in
        Lwt.return warnings

    let duplicateVersion book =
      let%lwt sets = unique_sets_from_contents book in
      let%lwt standalone_versions = versions_from_contents book in
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
        (
          fun v ->
            let%lwt tune = Version.tune v in
            register_tune_to_set tune None;
            Lwt.return ()
        )
        standalone_versions;%lwt
      (* register tunes in sets *)
      Lwt_list.iter_s
        (
          fun set ->
            let%lwt versions_and_parameters = Set.versions_and_parameters set in
            let versions = List.map fst versions_and_parameters in
            Lwt_list.iter_s
              (
                fun v ->
                  let%lwt tune = Version.tune v in
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
      |> Lwt_list.fold_left_s
        (
          fun warnings (tune, set_opts) ->
            let%lwt set_opts = List.sort_count_lwt (Option.compare_lwt Set.compare) set_opts in
            if List.length set_opts > 1 then
              Lwt.return ((DuplicateVersion (tune, set_opts)) :: warnings)
            else
              Lwt.return warnings
        )
        []

    let setDanceMismatch book =
      let%lwt sets_and_parameters = sets_and_parameters_from_contents book in
      Lwt_list.filter_map_p
        (
          fun (set, parameters) ->
            let%olwt dance_slug = Lwt.return (SetParameters.for_dance parameters) in
            (* FIXME: SetParameters should be hidden behind the same kind of
               mechanism as the rest; and this step should not be necessary *)
            let%lwt dance = Dance.get dance_slug in
            let%lwt dance_kind = Dance.kind dance in
            let%lwt set_kind = Set.kind set in
            if set_kind = dance_kind then
              Lwt.return_none
            else
              Lwt.return_some (SetDanceMismatch (set, dance))
        )
        sets_and_parameters

    let all book =
      Lwt_list.fold_left_s
        (
          fun warnings new_warnings_lwt ->
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
    | PageCore.Version (version, params) ->
      let%lwt version = Version.get version in
      Lwt.return @@ Version (version, params)
    | PageCore.Set (set, params) ->
      let%lwt set = Set.get set in
      Lwt.return @@ Set (set, params)
    | PageCore.InlineSet (set, params) ->
      Lwt.return @@ InlineSet (set, params)

  module Filter = struct
    include BookCore.Filter

    let accepts filter book =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@
      function
      | Is book' ->
        equal book book' >|=| Formula.interpret_bool
      | Title string ->
        let%lwt title = title book in
        Lwt.return (String.proximity ~char_equal string title)
      | TitleMatches string ->
        let%lwt title = title book in
        Lwt.return (String.inclusion_proximity ~char_equal ~needle: string title)
      | Subtitle string ->
        let%lwt subtitle = subtitle book in
        Lwt.return (String.proximity ~char_equal string subtitle)
      | SubtitleMatches string ->
        let%lwt subtitle = subtitle book in
        Lwt.return (String.inclusion_proximity ~char_equal ~needle: string subtitle)
      | IsSource ->
        is_source book >|=| Formula.interpret_bool
      | ExistsVersion vfilter ->
        let%lwt content = contents book in
        let%lwt versions =
          Lwt_list.filter_map_s
            (
              function
              | Version (v, _p) -> Lwt.return_some v
              | _ -> Lwt.return_none
            )
            content
        in
        Formula.interpret_exists (Version.Filter.accepts vfilter) versions
      | ExistsSet sfilter ->
        let%lwt content = contents book in
        let%lwt sets =
          Lwt_list.filter_map_s
            (
              function
              | Set (s, _p) -> Lwt.return_some s
              | _ -> Lwt.return_none
            )
            content
        in
        Formula.interpret_exists (Set.Filter.accepts sfilter) sets
      | ExistsInlineSet sfilter ->
        let%lwt content = contents book in
        let%lwt isets =
          Lwt_list.filter_map_s
            (
              function
              | InlineSet (s, _p) -> Lwt.return_some s
              | _ -> Lwt.return_none
            )
            content
        in
        Formula.interpret_exists (Set.Filter.accepts sfilter) isets

    let is book = Formula.pred (Is book)
    let title string = Formula.pred (Title string)
    let titleMatches string = Formula.pred (TitleMatches string)
    let subtitle string = Formula.pred (Subtitle string)
    let subtitleMatches string = Formula.pred (SubtitleMatches string)
    let isSource = Formula.pred IsSource
    let existsVersion vfilter = Formula.pred (ExistsVersion vfilter)
    let memVersion version = existsVersion (Version.Filter.is version)
    let existsSet sfilter = Formula.pred (ExistsSet sfilter)
    let memSet set = existsSet (Set.Filter.is set)
    let existsInlineSet sfilter = Formula.pred (ExistsInlineSet sfilter)

    let existsVersionDeep vfilter =
      Formula.or_l
        [
          existsVersion vfilter;
          existsSet (Set.Filter.existsVersion vfilter);
          existsInlineSet (Set.Filter.existsVersion vfilter);
        ]
    (** Check whether the given version filter can be satisfied in the book at any
        depth. This is different from {!existsVersion} which checks only in the
        direct list of version. *)

    let memVersionDeep version = existsVersionDeep (Version.Filter.is version)

    let existsTuneDeep tfilter = existsVersionDeep (Version.Filter.tune tfilter)
    (** Checks whether the given tune filter can be satisfied in the book at any
        depth. *)

    let memTuneDeep tune = existsTuneDeep (Tune.Filter.is tune)

    let raw string =
      Ok
        (
          Formula.or_l
            [
              titleMatches string;
              subtitleMatches string;
            ]
        )

    let nullary_text_predicates =
      [
        "source", isSource
      ]

    let unary_text_predicates =
      TextFormula.[
        "title", raw_only ~convert: no_convert title;
        "title-matches", raw_only ~convert: no_convert titleMatches;
        "subtitle", raw_only ~convert: no_convert subtitle;
        "subtitle-matches", raw_only ~convert: no_convert subtitleMatches;
        "exists-version", (existsVersion @@@@ Version.Filter.from_text_formula);
        "exists-set", (existsSet @@@@ Set.Filter.from_text_formula);
        "exists-inline-set", (existsInlineSet @@@@ Set.Filter.from_text_formula);
        "exists-version-deep", (existsVersionDeep @@@@ Version.Filter.from_text_formula);
        "exists-tune-deep", (existsVersionDeep @@@@ Version.Filter.from_text_formula);
      ]

    let from_text_formula =
      TextFormula.make_to_formula
        raw
        nullary_text_predicates
        unary_text_predicates

    let from_string ?filename input =
      from_text_formula (TextFormula.from_string ?filename input)
  end
end
