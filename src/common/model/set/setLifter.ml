open Nes
open Dancelor_common_database

module Lift
    (Person : module type of PersonSignature)
    (Dance : module type of DanceSignature)
    (Tune : module type of TuneSignature)
    (Version : module type of VersionSignature)
= struct
  include SetCore

  let make
      ?status
      ?(slug = Slug.none)
      ~name
      ?conceptors
      ~kind
      ?contents
      ~order
      ?dances
      ~modified_at
      ~created_at
      ()
    =
    let name = String.remove_duplicates ~char: ' ' name in
    let conceptors = Option.map (List.map Entry.slug) conceptors in
    let contents = Option.map (List.map (fun (version, parameters) -> (Entry.slug version, parameters))) contents in
    let dances = Option.map (List.map Entry.slug) dances in
    Lwt.return
      (
        Entry.make ~slug ?status ~modified_at ~created_at @@
        make_core ~name ?conceptors ~kind ?contents ~order ?dances ()
      )

  let is_slug_none : t -> bool = Slug.is_none % Entry.slug
  let conceptors = Lwt_list.map_p Person.get % conceptors
  let dances = Lwt_list.map_p Dance.get % dances

  let contents =
    Lwt_list.map_s
      (fun (slug, parameters) ->
         let%lwt version = Version.get slug in
         Lwt.return (version, parameters)
      ) %
    contents

  let compare : t -> t -> int =
    Slug.compare_slugs_or ~fallback: Stdlib.compare Entry.slug
  let equal set1 set2 = compare set1 set2 = 0

  (* FIXME: use Version.equal *)
  let contains_version slug1 set =
    List.exists
      (fun (slug2, _parameters) ->
         Slug.equal' slug1 slug2
      )
      (Entry.value set).contents

  let find_context index set =
    let%lwt versions = Lwt.map (List.map fst) @@ contents set in
    Lwt.return @@ List.findi_context (fun i _ -> i = index) versions

  let lilypond_content_cache_key set =
    let%lwt contents = contents set in
    let versions = List.map fst contents in
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
      let%lwt contents = contents s in
      Lwt.return (List.map fst contents)
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun version ->
           if Version.bars version <> bars then
             add_warning (WrongVersionBars version);
           let%lwt tune = Version.tune version in
           if Tune.kind tune <> kind then
             add_warning (WrongVersionKind tune);
           Lwt.return ()
        )
        versions
    in
    (* Check that there are no duplicates. *)
    let%lwt tunes = Lwt_list.map_s Version.tune versions in
    let tunes = List.sort Tune.compare tunes in
    (
      match tunes with
      | [] -> add_warning Empty
      | tune :: tunes ->
        let _ =
          List.fold_left
            (fun prev curr ->
               if prev = curr then
                 add_warning (DuplicateVersion curr);
               curr
            )
            tune
            tunes
        in
        ()
    );
    Lwt.return !warnings

  module Filter = struct
    include SetCore.Filter

    let accepts filter set =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
      | Is set' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug set) set'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ SetCore.name set
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ SetCore.name set
      | ExistsConceptor pfilter ->
        let%lwt conceptors = conceptors set in
        let%lwt scores = Lwt_list.map_s (Person.Filter.accepts pfilter) conceptors in
        Lwt.return (Formula.interpret_or_l scores)
      | ExistsVersion vfilter ->
        let%lwt contents = contents set in
        Formula.interpret_exists
          (fun (version, _) ->
             Version.Filter.accepts vfilter version
          )
          contents
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ SetCore.kind set

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            raw (Result.ok % nameMatches');
            unary_string ~name: "name" (name, unName);
            unary_string ~name: "name-matches" (nameMatches, unNameMatches);
            unary_lift ~name: "exists-conceptor" (existsConceptor, unExistsConceptor) ~converter: Person.Filter.text_formula_converter;
            unary_lift ~name: "by" (existsConceptor, unExistsConceptor) ~converter: Person.Filter.text_formula_converter; (* alias for exists-conceptor; FIXME: make this clearer *)
            unary_lift ~name: "exists-version" (existsVersion, unExistsVersion) ~converter: Version.Filter.text_formula_converter;
            unary_lift ~name: "kind" (kind, unKind) ~converter: Kind.Dance.Filter.text_formula_converter;
            unary_string ~name: "is" (is % Slug.unsafe_of_string, Option.map Slug.to_string % unIs);
          ]
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind (TextFormula.from_string ?filename input) from_text_formula

    let to_text_formula = TextFormula.of_formula text_formula_converter
    let to_string = TextFormula.to_string % to_text_formula

    let is = is % Entry.slug
    let is' = Formula.pred % is

    let memVersion = existsVersion % Version.Filter.is'
    let memVersion' = Formula.pred % memVersion

    (* Little trick to convince OCaml that polymorphism is OK. *)
    type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

    let optimise =
      let lift {op} f1 f2 =
        match (f1, f2) with
        | (ExistsConceptor f1, ExistsConceptor f2) -> Option.some @@ existsConceptor (op f1 f2)
        | (ExistsVersion f1, ExistsVersion f2) -> Option.some @@ existsVersion (op f1 f2)
        | (Kind f1, Kind f2) -> Option.some @@ kind (op f1 f2)
        | _ -> None
      in
      Formula.optimise
        ~lift_and: (lift {op = Formula.and_})
        ~lift_or: (lift {op = Formula.or_})
        (function
          | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
          | ExistsConceptor pfilter -> existsConceptor @@ Person.Filter.optimise pfilter
          | ExistsVersion vfilter -> existsVersion @@ Version.Filter.optimise vfilter
          | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
        )
  end
end
