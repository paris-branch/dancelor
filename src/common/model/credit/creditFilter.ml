open Nes

let _key = "credit-filter"

type predicate =
  | Is of CreditCore.t
  | Line of string
  | LineMatches of string
  | ExistsPerson of PersonFilter.t
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is credit = Formula.pred (Is credit)
let line line = Formula.pred (Line line)
let lineMatches line = Formula.pred (LineMatches line)
let existsPerson pfilter = Formula.pred (ExistsPerson pfilter)
let memPerson person = existsPerson (PersonFilter.is person)
let forallPersons pfilter = Formula.(not_ (existsPerson (not_ pfilter)))

let raw string = Ok (lineMatches string)

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "line",          raw_only ~convert:no_convert line;
    "line-matches",  raw_only ~convert:no_convert lineMatches;
    "exists-person", (existsPerson @@@@ PersonFilter.from_text_formula) (* FIXME *)
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
