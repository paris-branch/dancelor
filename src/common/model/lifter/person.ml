open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

module Lift () = struct
  include Dancelor_common_model_core.Person

  let make ~name ?scddb_id () = make ~name ~scddb_id ()

  let trad_slug : Dancelor_common_model_core.Person.t Slug.t = Slug.unsafe_of_string "traditional"
  let is_trad c = Slug.equal' (Entry.slug c) trad_slug

  let equal = Entry.equal'

  module Filter = struct
    include Dancelor_common_model_filter.Person

    let accepts filter person =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
      | Is person' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.unsafe_equal (Entry.slug person) person'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Dancelor_common_model_core.Person.name person
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Dancelor_common_model_core.Person.name person

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            raw (Result.ok % nameMatches');
            unary_string ~name: "name" (name, unName);
            unary_string ~name: "name-matches" (nameMatches, unNameMatches);
            unary_string ~name: "is" (is % Slug.unsafe_of_string, Option.map Slug.to_string % unIs);
          ]
      )

    let from_text_formula = TextFormulaConverter.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind (TextFormula.from_string ?filename input) from_text_formula

    let to_text_formula = TextFormula.of_formula text_formula_converter
    let to_string = TextFormula.to_string % to_text_formula

    let is = is % Entry.slug
    let is' = Formula.pred % is

    let optimise =
      Formula.optimise @@ function
      | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
  end
end
