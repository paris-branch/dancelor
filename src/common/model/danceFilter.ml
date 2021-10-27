open Nes

let _key = "dance-filter"

type predicate =
  | Is of DanceCore.t
  | Name of string
  | NameMatches of string
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is dance = Formula.pred (Is dance)
let name name = Formula.pred (Name name)
let nameMatches name = Formula.pred (NameMatches name)

let raw = nameMatches

let accepts filter dance =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Is dance' ->
    DanceCore.equal dance dance' >|=| Formula.interpret_bool

  | Name string ->
    let%lwt name = DanceCore.name dance in
    Lwt.return (String.proximity ~char_equal string name)

  | NameMatches string ->
    let%lwt name = DanceCore.name dance in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "name",         raw_only ~convert:Fun.id name;
    "name-matches", raw_only ~convert:Fun.id nameMatches
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
