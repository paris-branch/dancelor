open Nes

let _key = "credit"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    line : string ;
    persons : Person.t Slug.t list [@default []];
    scddb_id : int option [@default None] [@key "scddb-id"] }
[@@deriving yojson, make]

let slug c = Lwt.return c.slug
let status c = Lwt.return c.status
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons
let scddb_id c = Lwt.return c.scddb_id

let trad_slug = Slug.unsafe_of_string "traditional"
let is_trad c = Slug.equal c.slug trad_slug

let equal credit1 credit2 =
  let%lwt slug1 = slug credit1 in
  let%lwt slug2 = slug credit2 in
  Lwt.return (Slug.equal slug1 slug2)

module Filter = struct
  let _key = "credit-filter"

  type predicate =
    | Is of t
    | Line of string
    | LineMatches of string
    | ExistsPerson of Person.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is credit = Formula.pred (Is credit)
  let line line = Formula.pred (Line line)
  let lineMatches line = Formula.pred (LineMatches line)
  let existsPerson pfilter = Formula.pred (ExistsPerson pfilter)
  let memPerson person = existsPerson (Person.Filter.is person)
  let forallPersons pfilter = Formula.(not_ (existsPerson (not_ pfilter)))

  let raw = lineMatches

  let nullary_text_predicates = []

  let unary_text_predicates =
    TextFormula.[
      "line",          raw_only ~convert:Fun.id line;
      "line-matches",  raw_only ~convert:Fun.id lineMatches;
      "exists-person", (existsPerson @@@ Person.Filter.from_text_formula)
    ]

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end
