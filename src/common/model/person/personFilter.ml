open Nes

let _key = "person-filter"

type predicate =
  | Slug of PersonCore.t Slug.t
  | Name of string
  | NameMatches of string
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let slug person = Formula.pred (Slug person)
let is person = slug person.PersonCore.slug
let name name = Formula.pred (Name name)
let nameMatches name = Formula.pred (NameMatches name)

let raw string = Ok (nameMatches string)

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "name",         raw_only ~convert:no_convert name;
    "name-matches", raw_only ~convert:no_convert nameMatches
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
