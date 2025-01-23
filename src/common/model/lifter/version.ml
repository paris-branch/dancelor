open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

module Lift
    (Person : module type of Dancelor_common_model_signature.Person)
    (Tune : module type of Dancelor_common_model_signature.Tune)
= struct
  include Dancelor_common_model_core.Version

  let make
      ~tune
      ~bars
      ~key
      ~structure
      ?arrangers
      ?remark
      ?disambiguation
      ~content
      ()
    =
    let structure = String.remove_duplicates ~char: ' ' structure in
    let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
    let tune = Entry.slug tune in
    let arrangers = Option.map (List.map Entry.slug) arrangers in
    make ~tune ~bars ~key ~structure ?arrangers ?remark ?disambiguation ~content ()

  let tune = Tune.get % tune
  let arrangers = Lwt_list.map_p Person.get % arrangers

  let kind version =
    Fun.flip Lwt.map (tune version) @@ fun tune ->
    (bars version, Tune.kind tune)

  let name version = Lwt.map Tune.name (tune version)

  module Filter = struct
    let versionCore_tune = tune

    include Dancelor_common_model_filter.Version

    let accepts filter version =
      Formula.interpret filter @@ function
      | Is version' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug version) version'
      | Tune tfilter ->
        let%lwt tune = versionCore_tune version in
        Tune.Filter.accepts tfilter tune
      | Key key' ->
        Lwt.return @@ Formula.interpret_bool (Dancelor_common_model_core.Version.key version = key')
      | Kind kfilter ->
        let%lwt tune = versionCore_tune version in
        Kind.Version.Filter.accepts kfilter (Dancelor_common_model_core.Version.bars version, Tune.kind tune)

    let text_formula_converter =
      TextFormulaConverter.(
        merge
          ~tiebreaker: Left
          (
            (* Version-specific converter. *)
            make
              [
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

    let is = is % Entry.slug
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
          | (Is _ as p) | (Key _ as p) -> p
          | Tune tfilter -> tune @@ Tune.Filter.optimise tfilter
          | Kind kfilter -> kind @@ Kind.Version.Filter.optimise kfilter
        )
  end
end
