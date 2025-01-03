open Nes

module Lift
    (Person : module type of PersonSignature)
    (Tune : module type of TuneSignature)
= struct
  include VersionCore

  let make
      ~slug
      ?status
      ~tune
      ~bars
      ~key
      ~structure
      ?arrangers
      ?remark
      ?disambiguation
      ?broken
      ~modified_at
      ~created_at
      ()
    =
    let structure = String.remove_duplicates ~char: ' ' structure in
    let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
    let tune = Tune.slug tune in
    let arrangers = Option.map (List.map Person.slug) arrangers in
    Lwt.return
      (
        make
          ~slug
          ?status
          ~tune
          ~bars
          ~key
          ~structure
          ?arrangers
          ?remark
          ?disambiguation
          ?broken
          ~modified_at
          ~created_at
          ()
      )

  let tune version = Tune.get (tune version)
  let arrangers version = Lwt_list.map_p Person.get version.arrangers

  let set_broken t broken = {t with broken}

  let kind version =
    Fun.flip Lwt.map (tune version) @@ fun tune ->
    (bars version, Tune.kind tune)

  let name version = Lwt.map Tune.name (tune version)

  module Filter = struct
    let versionCore_tune = tune

    include VersionCore.Filter

    let accepts filter version =
      Formula.interpret filter @@ function
      | Is version' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (slug version) version'
      | Tune tfilter ->
        let%lwt tune = versionCore_tune version in
        Tune.Filter.accepts tfilter tune
      | Key key' ->
        Lwt.return @@ Formula.interpret_bool (VersionCore.key version = key')
      | Kind kfilter ->
        let%lwt tune = versionCore_tune version in
        Kind.Version.Filter.accepts kfilter (VersionCore.bars version, Tune.kind tune)
      | Broken ->
        Lwt.return @@ Formula.interpret_bool @@ VersionCore.broken version

    let text_formula_converter =
      TextFormulaConverter.(
        merge
          ~tiebreaker: Left
          (
            (* Version-specific converter. *)
            make
              [
                nullary ~name: "broken" broken;
                unary_lift ~wrap_back: NotRaw ~name: "tune" (tune, unTune) ~converter: Tune.Filter.text_formula_converter;
                unary_raw ~name: "key" (key, unKey) ~cast: (Music.key_of_string_opt, Music.key_to_string) ~type_: "key";
                unary_lift ~name: "kind" (kind, unKind) ~converter: Kind.Version.Filter.text_formula_converter;
                unary_string ~name: "is" (is % Slug.unsafe_of_string, Option.map Slug.to_string % unIs);
              ]
          )
          (
            (* Tune converter, lifted to versions. Lose in case of tiebreak. *)
            map tune Tune.Filter.text_formula_converter ~error: ((^) "As tune lifted to version: ")
          )
      )

    let from_text_formula = TextFormula.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind
        (TextFormula.from_string ?filename input)
        from_text_formula

    let to_text_formula = TextFormula.of_formula text_formula_converter
    let to_string = TextFormula.to_string % to_text_formula

    let is = is % slug
    let is' = Formula.pred % is

    let tuneIs = tune % Tune.Filter.is'
    let tuneIs' = Formula.pred % tuneIs

    (* Little trick to convince OCaml that polymorphism is OK. *)
    type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

    let optimise =
      let lift {op} f1 f2 =
        match (f1, f2) with
        | (Tune f1, Tune f2) -> Option.some @@ tune (op f1 f2)
        | (Kind f1, Kind f2) -> Option.some @@ kind (op f1 f2)
        | _ -> None
      in
      Formula.optimise
        ~lift_and: (lift {op = Formula.and_})
        ~lift_or: (lift {op = Formula.or_})
        (function
          | (Is _ as p) | (Key _ as p) | (Broken as p) -> p
          | Tune tfilter -> tune @@ Tune.Filter.optimise tfilter
          | Kind kfilter -> kind @@ Kind.Version.Filter.optimise kfilter
        )
  end
end
