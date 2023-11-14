open Nes

module Lift () = struct
  include PersonCore

  let make
      ~slug ?status ~line ?scddb_id ~modified_at ~created_at
      ()
    =
    let line = String.remove_duplicates ~char:' ' line in
    make ~slug ?status ~line ?scddb_id ~modified_at ~created_at ()

  let line c = Lwt.return c.line
  let scddb_id c = Lwt.return c.scddb_id

  let trad_slug = Slug.unsafe_of_string "traditional"
  let is_trad c = Slug.equal c.slug trad_slug

  let equal person1 person2 =
    let%lwt slug1 = slug person1 in
    let%lwt slug2 = slug person2 in
    Lwt.return (Slug.equal slug1 slug2)

  module Filter = struct
    include PersonCore.Filter

    let accepts filter person =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function

      | Is person' ->
        equal person person' >|=| Formula.interpret_bool

      | Line string ->
        let%lwt line = line person in
        Lwt.return (String.proximity ~char_equal string line)

      | LineMatches string ->
        let%lwt line = line person in
        Lwt.return (String.inclusion_proximity ~char_equal ~needle:string line)

    let is person = Formula.pred (Is person)
    let line line = Formula.pred (Line line)
    let lineMatches line = Formula.pred (LineMatches line)

    let raw string = Ok (lineMatches string)

    let nullary_text_predicates = []

    let unary_text_predicates =
      TextFormula.[
        "line",          raw_only ~convert:no_convert line;
        "line-matches",  raw_only ~convert:no_convert lineMatches;
      ]

    let from_text_formula =
      TextFormula.make_to_formula raw
        nullary_text_predicates
        unary_text_predicates

    let from_string ?filename input =
      from_text_formula (TextFormula.from_string ?filename input)
  end
end
