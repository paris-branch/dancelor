open Nes
open Dancelor_common_database


module Lift
    (Dance : Signature.Dance.S)
    (Person : Signature.Person.S)
= struct
  include Core.Tune

  let make
      ~name
      ?alternative_names
      ~kind
      ?composers
      ?dances
      ?remark
      ?scddb_id
      ?date
      ()
    =
    let name = String.remove_duplicates ~char: ' ' name in
    let alternative_names = Option.map (List.map (String.remove_duplicates ~char: ' ')) alternative_names in
    let composers = Option.map (List.map Entry.slug) composers in
    let dances = Option.map (List.map Entry.slug) dances in
    make ~name ?alternative_names ~kind ?composers ?dances ?remark ~scddb_id ~date ()

  let composers = Lwt_list.map_p Person.get % composers
  let dances = Lwt_list.map_p Dance.get % dances

  module Filter = struct
    (* NOTE: [include Core.Tune.Filter] shadows the accessors of [Dancelor_common_model_core.Tune]. *)
    let tuneCore_dances = dances

    include Filter.Tune

    let accepts filter tune =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
      | Is tune' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug tune) tune'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Core.Tune.name tune
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Core.Tune.name tune
      | ExistsComposer pfilter ->
        let%lwt composers = composers tune in
        let%lwt scores = Lwt_list.map_s (Person.Filter.accepts pfilter) composers in
        Lwt.return (Formula.interpret_or_l scores)
      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ Core.Tune.kind tune
      | ExistsDance dfilter ->
        let%lwt dances = tuneCore_dances tune in
        let%lwt scores = Lwt_list.map_s (Dance.Filter.accepts dfilter) dances in
        Lwt.return (Formula.interpret_or_l scores)

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            raw (Result.ok % nameMatches');
            unary_string ~name: "name" (name, unName);
            unary_string ~wrap_back: Never ~name: "name-matches" (nameMatches, unNameMatches);
            unary_lift ~name: "exists-composer" (existsComposer, unExistsComposer) ~converter: Person.Filter.text_formula_converter;
            unary_lift ~name: "by" (existsComposer, unExistsComposer) ~converter: Person.Filter.text_formula_converter; (* alias for exists-composer; FIXME: make this clearer *)
            unary_lift ~name: "kind" (kind, unKind) ~converter: Kind.Base.Filter.text_formula_converter;
            unary_lift ~name: "exists-dance" (existsDance, unExistsDance) ~converter: Dance.Filter.text_formula_converter;
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

    let existsComposerIs = existsComposer % Person.Filter.is'
    let existsComposerIs' = Formula.pred % existsComposerIs

    (* Little trick to convince OCaml that polymorphism is OK. *)
    type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

    let optimise =
      let lift {op} f1 f2 =
        match (f1, f2) with
        | (ExistsComposer f1, ExistsComposer f2) -> Option.some @@ existsComposer (op f1 f2)
        | (Kind f1, Kind f2) -> Option.some @@ kind (op f1 f2)
        | (ExistsDance f1, ExistsDance f2) -> Option.some @@ existsDance (op f1 f2)
        | _ -> None
      in
      Formula.optimise
        ~lift_and: (lift {op = Formula.and_})
        ~lift_or: (lift {op = Formula.or_})
        (function
          | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
          | ExistsComposer pfilter -> existsComposer @@ Person.Filter.optimise pfilter
          | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
          | ExistsDance dfilter -> existsDance @@ Dance.Filter.optimise dfilter
        )
  end
end
