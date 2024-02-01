open Nes

module Lift
    (Person  : module type of  PersonSignature)
    (Dance   : module type of   DanceSignature)
    (Tune    : module type of    TuneSignature)
    (Version : module type of VersionSignature)
= struct
  include SetCore

  let make
      ?status ?(slug=Slug.none) ~name ?deviser ~kind ?versions_and_parameters
      ~order ?dances ~modified_at ~created_at
      ()
    =
    let name = String.remove_duplicates ~char:' ' name in
    let deviser = Option.map Person.slug deviser in
    let versions_and_parameters = Option.map (List.map (fun (version, parameters) -> (Version.slug version, parameters))) versions_and_parameters in
    let dances = Option.map (List.map Dance.slug) dances in
    Lwt.return (make ?status ~slug ~name ~deviser ~kind ?versions_and_parameters
                  ~order ?dances ~modified_at ~created_at
                  ())

  let is_slug_none = Slug.is_none % slug
  let deviser = Option.fold ~none:Lwt.return_none ~some:(Lwt.map Option.some % Person.get) % deviser
  let dances = Lwt_list.map_p Dance.get % dances

  let versions_and_parameters set =
    Lwt_list.map_s
      (fun (slug, parameters) ->
         let%lwt version = Version.get slug in
         Lwt.return (version, parameters))
      set.versions_and_parameters

  let compare = Slug.compare_slugs_or ~fallback:Stdlib.compare slug
  let equal set1 set2 = compare set1 set2 = 0

  (* FIXME: use Version.equal *)
  let contains_version slug1 set =
    List.exists
      (fun (slug2, _parameters) ->
         Slug.equal slug1 slug2)
      set.versions_and_parameters

  let lilypond_content_cache_key set =
    let%lwt versions_and_parameters = versions_and_parameters set in
    let versions = List.map fst versions_and_parameters in
    let%lwt contents = Lwt_list.map_p Version.content versions in
    Lwt.return (String.concat "\n" contents)

  let warnings s =
    let warnings = ref [] in
    let add_warning w = warnings := w :: !warnings in
    (* Check that version kinds and bars correspond to set's kind. *)
    let (bars, kind) =
      match kind s with
      | Mul (_, Version (bars, kind)) -> (bars, kind)
      | _ ->
        (* FIXME: more complicated that it appears: For sets that have a medley
           kind, checking that “the version has a compatible kind” makes little
           sense. I don't think there is a very good solution right now; ideally
           later we should check that the versions “added” as per the set's
           order sum up to the kind of the set, but that's more involved. *)
        (32, KindBase.Reel)
    in
    let%lwt versions =
      let%lwt versions_and_parameters = versions_and_parameters s in
      Lwt.return (List.map fst versions_and_parameters)
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun version ->
           if Version.bars version <> bars then
             add_warning (WrongVersionBars version);
           let%lwt tune = Version.tune version in
           if Tune.kind tune <> kind then
             add_warning (WrongVersionKind tune);
           Lwt.return ())
        versions
    in
    (* Check that there are no duplicates. *)
    let%lwt tunes = Lwt_list.map_s Version.tune versions in
    let tunes = List.sort Tune.compare tunes in
    (match tunes with
     | [] -> add_warning Empty
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
    Lwt.return !warnings

  module Filter = struct
    include SetCore.Filter

    let accepts filter set =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is set' ->
        Lwt.return @@ Formula.interpret_bool @@ equal set set'

      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ name set

      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ name set

      | Deviser dfilter ->
        (match%lwt deviser set with
         | None -> Lwt.return Formula.interpret_false
         | Some deviser -> Person.Filter.accepts dfilter deviser)

      | ExistsVersion vfilter ->
        let%lwt versions_and_parameters = versions_and_parameters set in
        Formula.interpret_exists
          (fun (version, _) ->
             Version.Filter.accepts vfilter version)
          versions_and_parameters

      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ kind set

    let is set = Formula.pred (Is set)
    let name name = Formula.pred (Name name)
    let nameMatches name = Formula.pred (NameMatches name)
    let deviser cfilter = Formula.pred (Deviser cfilter)
    let existsVersion vfilter = Formula.pred (ExistsVersion vfilter)
    let memVersion version = existsVersion (Version.Filter.is version)
    let kind kfilter = Formula.pred (Kind kfilter)

    let raw = Result.ok % nameMatches

    let nullary_text_predicates =
      TextFormula.[
        nullary ~name:"reel"       (kind Kind.(Dance.Filter.base Base.(Filter.is Reel)));       (* alias for kind:reel       FIXNE: make this clearer *)
        nullary ~name:"jig"        (kind Kind.(Dance.Filter.base Base.(Filter.is Jig)));        (* alias for kind:jig        FIXNE: make this clearer *)
        nullary ~name:"strathspey" (kind Kind.(Dance.Filter.base Base.(Filter.is Strathspey))); (* alias for kind:strathspey FIXNE: make this clearer *)
        nullary ~name:"waltz"      (kind Kind.(Dance.Filter.base Base.(Filter.is Waltz)));      (* alias for kind:waltz      FIXNE: make this clearer *)
      ]

    let unary_text_predicates =
      TextFormula.[
        unary ~name:"name"           (raw_only ~convert:no_convert name);
        unary ~name:"name-matches"   (raw_only ~convert:no_convert nameMatches);
        unary ~name:"deviser"        (deviser @@@@ Person.Filter.from_text_formula);
        unary ~name:"by"             (deviser @@@@ Person.Filter.from_text_formula); (* alias for deviser; FIXME: make this clearer *)
        unary ~name:"exists-version" (existsVersion @@@@ Version.Filter.from_text_formula);
        unary ~name:"kind"           (kind @@@@ Kind.Dance.Filter.from_text_formula);
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
