open Nes

module Lift
    (Dance   : module type of   DanceSignature)
    (Set     : module type of     SetSignature)
    (Tune    : module type of    TuneSignature)
    (Version : module type of VersionSignature)
= struct
  include BookCore

  let short_title book = if short_title book = "" then title book else short_title book

  let is_source book = source book

  let compare =
    Slug.compare_slugs_or
      ~fallback:
        (fun book1 book2 ->
           (* Compare first by date *)
           let c = compare book1.date book2.date in
           if c = 0 then
             compare book1 book2
           else
             c)
      slug

  let equal book1 book2 = compare book1 book2 = 0

  let contents book =
    Lwt_list.map_p
      (function
        | PageCore.Version (version, parameters) ->
          let%lwt version = Version.get version in
          Lwt.return (Version (version, parameters))
        | PageCore.Set (set, parameters) ->
          let%lwt set = Set.get set in
          Lwt.return (Set (set, parameters))
        | PageCore.InlineSet (set, parameters) ->
          Lwt.return (InlineSet (set, parameters)))
      (contents book)

  let versions_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (function
        | Version (version, _) -> Lwt.return_some version
        | _ -> Lwt.return_none)
      contents

  let sets_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (function
        | Version _ -> Lwt.return_none
        | Set (set, _) | InlineSet (set, _) -> Lwt.return_some set)
      contents

  let unique_sets_from_contents =
    Lwt.map (List.sort_uniq Set.compare) % sets_from_contents

  let sets_and_parameters_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (function
        | Set (set, parameters) | InlineSet (set, parameters) ->
          Lwt.return_some (set, parameters)
        | Version _ -> Lwt.return_none)
      contents

  let isInlineSet = function
    | InlineSet _ -> true
    | _ -> false

  let find_context_no_inline index set =
    let%lwt contents = contents set in
    let contents_no_inline = List.filter (not % isInlineSet) contents in
    Lwt.return @@ List.findi_context (fun i _ -> i = index) contents_no_inline

  let lilypond_contents_cache_key book =
    let%lwt pages = contents book in
    let%lwt contents = Lwt_list.map_p
        (function
          | Version (version, _) -> Version.content version
          | Set (set, _) | InlineSet (set, _) -> Set.lilypond_content_cache_key set)
        pages
    in
    Lwt.return (String.concat "\n" contents)

  let page_to_page_core = function
    | (Version (version, params) : page) -> PageCore.Version (Version.slug version, params)
    | (Set (set, params) : page) -> PageCore.Set (Set.slug set, params)
    | (InlineSet (set, params) : page) -> PageCore.InlineSet (set, params)

  let make ?status ~slug ~title ?date ?contents ~modified_at ~created_at () =
    let contents = Option.map (List.map page_to_page_core) contents in
    Lwt.return @@ make ?status ~slug ~title ?date ?contents ~modified_at ~created_at ()

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
      Fun.flip Lwt.map (sets_from_contents book) @@ fun sets ->
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
               (current_set, warnings))
            (first_set, [])
            other_sets
        in
        warnings

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
        (fun v ->
           let%lwt tune = Version.tune v in
           register_tune_to_set tune None;
           Lwt.return ())
        standalone_versions;%lwt
      (* register tunes in sets *)
      Lwt_list.iter_s
        (fun set ->
           let%lwt contents = Set.contents set in
           let versions = List.map fst contents in
           Lwt_list.iter_s
             (fun v ->
                let%lwt tune = Version.tune v in
                register_tune_to_set tune (Some set);
                Lwt.return ())
             versions)
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
             warnings)
        []
      |> Lwt.return

    let setDanceMismatch book =
      let%lwt sets_and_parameters = sets_and_parameters_from_contents book in
      Lwt_list.filter_map_p
        (fun (set, parameters) ->
           let%olwt dance_slug = Lwt.return (SetParameters.for_dance parameters) in
           (* FIXME: SetParameters should be hidden behind the same kind of
              mechanism as the rest; and this step should not be necessary *)
           let%lwt dance = Dance.get dance_slug in
           if Set.kind set = Dance.kind dance then
             Lwt.return_none
           else
             Lwt.return_some (SetDanceMismatch (set, dance)))
        sets_and_parameters

    let all book =
      Lwt_list.fold_left_s
        (fun warnings new_warnings_lwt ->
           let%lwt new_warnings = new_warnings_lwt in
           Lwt.return (warnings @ new_warnings))
        []
        [ empty book;
          duplicateSet book;
          duplicateVersion book;
          setDanceMismatch book ]
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

    let rec accepts filter book =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is book' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (slug book) book'

      | Title string ->
        Lwt.return @@ String.proximity ~char_equal string @@ BookCore.title book

      | TitleMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ BookCore.title book

      | Subtitle string ->
        Lwt.return @@ String.proximity ~char_equal string @@ BookCore.subtitle book

      | SubtitleMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ BookCore.subtitle book

      | IsSource ->
        Lwt.return @@ Formula.interpret_bool @@ is_source book

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

      | ExistsVersionDeep vfilter ->
        (* recursive call to check the compound formula *)
        Fun.flip accepts book @@ Formula.or_l [
          Formula.pred (ExistsVersion vfilter);
          Formula.pred (ExistsSet (Set.Filter.existsVersion' vfilter));
          Formula.pred (ExistsInlineSet (Set.Filter.existsVersion' vfilter));
        ]

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            raw (fun string -> Ok (Formula.or_ (titleMatches' string) (subtitleMatches' string)));
            unary_string ~name:"title"               (title, unTitle);
            unary_string ~name:"title-matches"       (titleMatches, unTitleMatches);
            unary_string ~name:"subtitle"            (subtitle, unSubtitle);
            unary_string ~name:"subtitle-matches"    (subtitleMatches, unSubtitleMatches);
            unary_lift   ~name:"exists-version"      (existsVersion, unExistsVersion)          ~converter:Version.Filter.text_formula_converter;
            unary_lift   ~name:"exists-set"          (existsSet, unExistsSet)                  ~converter:Set.Filter.text_formula_converter;
            unary_lift   ~name:"exists-inline-set"   (existsInlineSet, unExistsInlineSet)      ~converter:Set.Filter.text_formula_converter;
            unary_lift   ~name:"exists-version-deep" (existsVersionDeep, unExistsVersionDeep)  ~converter:Version.Filter.text_formula_converter;
            unary_string ~name:"is"           (is % Slug.unsafe_of_string, Option.map Slug.to_string % unIs);
            nullary      ~name:"is-source"            IsSource;
          ]
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind (TextFormula.from_string ?filename input) from_text_formula

    let to_text_formula = TextFormula.of_formula text_formula_converter
    let to_string = TextFormula.to_string % to_text_formula

    let is = is % slug
    let is' = Formula.pred % is

    let memVersion = existsVersion % Version.Filter.is'
    let memSet = existsSet % Set.Filter.is'
    let memVersionDeep = existsVersionDeep % Version.Filter.is'
    let existsTuneDeep = existsVersionDeep % Version.Filter.tune'
    let memTuneDeep = existsTuneDeep % Tune.Filter.is'

    let memVersion' = Formula.pred % memVersion
    let memSet' = Formula.pred % memSet
    let existsVersionDeep' = Formula.pred % existsVersionDeep
    let memVersionDeep' = Formula.pred % memVersionDeep
    let existsTuneDeep' = Formula.pred % existsTuneDeep
    let memTuneDeep' = Formula.pred % memTuneDeep

    (* Little trick to convince OCaml that polymorphism is OK. *)
    type op = { op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t }

    let optimise =
      let lift {op} f1 f2 = match (f1, f2) with
        | (ExistsVersion f1, ExistsVersion f2) -> Option.some @@ existsVersion (op f1 f2)
        | (ExistsSet f1, ExistsSet f2) -> Option.some @@ existsSet (op f1 f2)
        | (ExistsInlineSet f1, ExistsInlineSet f2) -> Option.some @@ existsInlineSet (op f1 f2)
        | (ExistsVersionDeep f1, ExistsVersionDeep f2) -> Option.some @@ existsVersionDeep (op f1 f2)
        | _ -> None
      in
      Formula.optimise
        ~lift_and: (lift {op = Formula.and_})
        ~lift_or: (lift {op = Formula.or_})
        (function
          | (Is _ as p) | (Title _ as p) | (TitleMatches _ as p) | (Subtitle _ as p)
          | (SubtitleMatches _ as p) | (IsSource as p) -> p
          | ExistsVersion vfilter -> existsVersion @@ Version.Filter.optimise vfilter
          | ExistsSet sfilter -> existsSet @@ Set.Filter.optimise sfilter
          | ExistsInlineSet sfilter -> existsInlineSet @@ Set.Filter.optimise sfilter
          | ExistsVersionDeep vfilter -> existsVersionDeep @@ Version.Filter.optimise vfilter)
  end
end
