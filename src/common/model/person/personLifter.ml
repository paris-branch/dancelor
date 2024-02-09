open Nes

module Lift () = struct
  include PersonCore

  let make
      ~slug ?status ~name ?scddb_id ~modified_at ~created_at
      ()
    =
    let name = String.remove_duplicates ~char:' ' name in
    make ~slug ?status ~name ?scddb_id ~modified_at ~created_at ()

  let trad_slug = Slug.unsafe_of_string "traditional"
  let is_trad c = Slug.equal c.slug trad_slug

  let equal person1 person2 =
    Slug.equal (slug person1) (slug person2)

  module Filter = struct
    include PersonCore.Filter

    let accepts filter person =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is person' ->
        Lwt.return @@ Formula.interpret_bool @@ equal person person'

      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ PersonCore.name person

      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle:string @@ PersonCore.name person

    let text_formula_converter =
      TextFormulaConverter.(
        make
          [
            raw (Result.ok % nameMatches');
            unary_string ~name:"name"         (name, unName);
            unary_string ~name:"name-matches" (nameMatches, unNameMatches);
          ]
      )

    let from_text_formula = TextFormulaConverter.to_formula text_formula_converter
    let from_string ?filename input =
      Result.bind (TextFormula.from_string ?filename input) from_text_formula
  end
end
