open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

module Lift
    (Person : Dancelor_common_model_signature.Person.S)
= struct
  include Dancelor_common_model_core.Dance

  let make
      ~name
      ~kind
      ?devisers
      ?two_chords
      ?scddb_id
      ?disambiguation
      ?date
      ()
    =
    let name = String.remove_duplicates ~char: ' ' name in
    let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
    let devisers = Option.map (List.map Entry.slug) devisers in
    make ~name ~kind ?devisers ~two_chords ~scddb_id ?disambiguation ~date ()

  let devisers = Lwt_list.map_p Person.get % devisers

  let equal dance1 dance2 = Slug.equal' (Entry.slug dance1) (Entry.slug dance2)

  module Filter = struct
    include Dancelor_common_model_filter.Dance

    let accepts filter dance =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
      | Is dance' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug dance) dance'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Dancelor_common_model_core.Dance.name dance
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Dancelor_common_model_core.Dance.name dance
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Dancelor_common_model_core.Dance.kind dance
      | ExistsDeviser pfilter ->
        let%lwt devisers = devisers dance in
        let%lwt scores = Lwt_list.map_s (Person.Filter.accepts pfilter) devisers in
        Lwt.return (Formula.interpret_or_l scores)

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            raw (Result.ok % nameMatches');
            unary_string ~name: "name" (name, unName);
            unary_string ~name: "name-matches" (nameMatches, unNameMatches);
            unary_lift ~name: "kind" (kind, unKind) ~converter: Kind.Dance.Filter.text_formula_converter;
            unary_lift ~name: "exists-deviser" (existsDeviser, unExistsDeviser) ~converter: Person.Filter.text_formula_converter;
            unary_lift ~name: "by" (existsDeviser, unExistsDeviser) ~converter: Person.Filter.text_formula_converter; (* alias for deviser; FIXME: make this clearer *)
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

    (* Little trick to convince OCaml that polymorphism is OK. *)
    type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

    let optimise =
      let lift {op} f1 f2 =
        match (f1, f2) with
        | (Kind f1, Kind f2) -> Option.some @@ kind (op f1 f2)
        | (ExistsDeviser f1, ExistsDeviser f2) -> Option.some @@ existsDeviser (op f1 f2)
        | _ -> None
      in
      Formula.optimise
        ~lift_and: (lift {op = Formula.and_})
        ~lift_or: (lift {op = Formula.or_})
        (function
          | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
          | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
          | ExistsDeviser pfilter -> existsDeviser @@ Person.Filter.optimise pfilter
        )
  end
end
