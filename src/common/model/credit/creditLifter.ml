open Nes

module Lift
    (Person : module type of PersonSignature)
= struct
  include CreditCore

  let make
      ~slug ?status ~line ?persons ?scddb_id ~modified_at ~created_at
      ()
    =
    let line = String.remove_duplicates ~char:' ' line in
    make ~slug ?status ~line ?persons ?scddb_id ~modified_at ~created_at ()

  let slug c = Lwt.return c.slug
  let status c = Lwt.return c.status
  let line c = Lwt.return c.line
  let persons credit = Lwt_list.map_p Person.get credit.persons
  let scddb_id c = Lwt.return c.scddb_id

  let trad_slug = Slug.unsafe_of_string "traditional"
  let is_trad c = Slug.equal c.slug trad_slug

  let equal credit1 credit2 =
    let%lwt slug1 = slug credit1 in
    let%lwt slug2 = slug credit2 in
    Lwt.return (Slug.equal slug1 slug2)

  module Filter = struct
    include CreditCore.Filter

    let is credit = Formula.pred (Is credit)
    let line line = Formula.pred (Line line)
    let lineMatches line = Formula.pred (LineMatches line)
    let existsPerson pfilter = Formula.pred (ExistsPerson pfilter)
    let memPerson person = existsPerson (Person.Filter.is person)
    let forallPersons pfilter = Formula.(not_ (existsPerson (not_ pfilter)))

    let raw string = Ok (lineMatches string)

    let nullary_text_predicates = []

    let unary_text_predicates =
      TextFormula.[
        "line",          raw_only ~convert:no_convert line;
        "line-matches",  raw_only ~convert:no_convert lineMatches;
        "exists-person", (existsPerson @@@@ Person.Filter.from_text_formula) (* FIXME *)
      ]

    let from_text_formula =
      TextFormula.make_to_formula raw
        nullary_text_predicates
        unary_text_predicates
  end
end
