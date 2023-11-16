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
    let%lwt versions_and_parameters =
      let%olwt versions_and_parameters = Lwt.return versions_and_parameters in
      let%lwt versions_and_parameters =
        Lwt_list.map_s
          (fun (version, parameters) ->
             let%lwt slug = Version.slug version in
             Lwt.return (slug, parameters))
          versions_and_parameters
      in
      Lwt.return_some versions_and_parameters
    in
    let dances = Option.map (List.map Dance.slug) dances in
    Lwt.return (make ?status ~slug ~name ~deviser ~kind ?versions_and_parameters
                  ~order ?dances ~modified_at ~created_at
                  ())

  let is_slug_none s =
    let%lwt slug = slug s in
    Lwt.return (Slug.is_none slug)

  let name s = Lwt.return s.name
  let deviser set = Olwt.flip @@ Option.map Person.get set.deviser
  let kind s = Lwt.return s.kind
  let order s = Lwt.return s.order
  let instructions s = Lwt.return s.instructions
  let dances set = Lwt_list.map_p Dance.get set.dances
  let remark set = Lwt.return set.remark
  let modified_at set = Lwt.return set.modified_at
  let created_at set = Lwt.return set.created_at

  let versions_and_parameters set =
    Lwt_list.map_s
      (fun (slug, parameters) ->
         let%lwt version = Version.get slug in
         Lwt.return (version, parameters))
      set.versions_and_parameters

  let compare =
    Slug.compare_slugs_or_lwt
      ~fallback:(fun set1 set2 ->
          Lwt.return (Stdlib.compare set1 set2))
      slug

  let equal = equal_from_compare compare

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
    let%lwt (bars, kind) =
      match%lwt kind s with
      | Mul (_, Version (bars, kind)) -> Lwt.return (bars, kind)
      | _ ->
        (* FIXME: more complicated that it appears: For sets that have a medley
           kind, checking that “the version has a compatible kind” makes little
           sense. I don't think there is a very good solution right now; ideally
           later we should check that the versions “added” as per the set's
           order sum up to the kind of the set, but that's more involved. *)
        Lwt.return (32, KindBase.Reel)
    in
    let%lwt versions =
      let%lwt versions_and_parameters = versions_and_parameters s in
      Lwt.return (List.map fst versions_and_parameters)
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun version ->
           let%lwt version_bars = Version.bars version in
           if version_bars <> bars then
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
        equal set set' >|=| Formula.interpret_bool

      | Name string ->
        let%lwt name = name set in
        Lwt.return (String.proximity ~char_equal string name)

      | NameMatches string ->
        let%lwt name = name set in
        Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

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
        let%lwt kind = kind set in
        Kind.Dance.Filter.accepts kfilter kind

    let is set = Formula.pred (Is set)
    let name name = Formula.pred (Name name)
    let nameMatches name = Formula.pred (NameMatches name)
    let deviser cfilter = Formula.pred (Deviser cfilter)
    let existsVersion vfilter = Formula.pred (ExistsVersion vfilter)
    let memVersion version = existsVersion (Version.Filter.is version)
    let kind kfilter = Formula.pred (Kind kfilter)

    let raw string = Ok (nameMatches string)

    let nullary_text_predicates = [
      "reel",       (kind Kind.(Dance.Filter.base Base.(Filter.is Reel)));       (* alias for kind:reel       FIXNE: make this clearer *)
      "jig",        (kind Kind.(Dance.Filter.base Base.(Filter.is Jig)));        (* alias for kind:jig        FIXNE: make this clearer *)
      "strathspey", (kind Kind.(Dance.Filter.base Base.(Filter.is Strathspey))); (* alias for kind:strathspey FIXNE: make this clearer *)
      "waltz",      (kind Kind.(Dance.Filter.base Base.(Filter.is Waltz)));      (* alias for kind:waltz      FIXNE: make this clearer *)
    ]

    let unary_text_predicates =
      TextFormula.[
        "name",           raw_only ~convert:no_convert name;
        "name-matches",   raw_only ~convert:no_convert nameMatches;
        "deviser",        (deviser @@@@ Person.Filter.from_text_formula);
        "by",             (deviser @@@@ Person.Filter.from_text_formula); (* alias for deviser; FIXME: make this clearer *)
        "exists-version", (existsVersion @@@@ Version.Filter.from_text_formula);
        "kind",           (kind @@@@ Kind.Dance.Filter.from_text_formula);
      ]

    let from_text_formula =
      TextFormula.make_to_formula raw
        nullary_text_predicates
        unary_text_predicates

    let from_string ?filename input =
      from_text_formula (TextFormula.from_string ?filename input)
  end
end
