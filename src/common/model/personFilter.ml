open Nes

let _key = "person-filter"

type predicate =
  | Is of PersonCore.t
  | Name of string
  | NameMatches of string
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is person = Formula.pred (Is person)
let name name = Formula.pred (Name name)
let nameMatches name = Formula.pred (NameMatches name)

let raw string = Ok (nameMatches string)

let accepts filter person =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Is person' ->
    PersonCore.equal person person' >|=| Formula.interpret_bool

  | Name string ->
    let%lwt name = PersonCore.name person in
    Lwt.return (String.proximity ~char_equal string name)

  | NameMatches string ->
    let%lwt name = PersonCore.name person in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

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
