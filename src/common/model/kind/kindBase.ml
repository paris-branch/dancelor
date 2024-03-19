open Nes

let _key = "kind-base"

type t =
  | Jig
  | Polka
  | Reel
  | Strathspey
  | Waltz
[@@deriving eq, show {with_path = false}]

let all = [Jig; Reel; Strathspey; Polka; Waltz]

let to_char = function
  | Jig -> 'J'
  | Polka -> 'P'
  | Reel -> 'R'
  | Strathspey -> 'S'
  | Waltz -> 'W'

let of_char c =
  match Char.uppercase_ascii c with
  | 'J' -> Jig
  | 'P' -> Polka
  | 'R' -> Reel
  | 'S' -> Strathspey
  | 'W' -> Waltz
  | _ -> invalid_arg "Dancelor_common_model.Kind.base_of_char"

let to_string b =
  String.make 1 (to_char b)

let of_string s =
  try of_char s.[0]
  with Invalid_argument _ | Failure _ ->
    invalid_arg "Dancelor_common_model.Kind.base_of_string"

let of_string_opt s =
  try Some (of_string s)
  with Invalid_argument _ -> None

let to_yojson b =
  `String (to_string b)

let of_yojson = function
  | `String s ->
    (try Ok (of_string s)
     with _ -> Error "Dancelor_common_model.Kind.base_of_yojson: not a valid base kind")
  | _ -> Error "Dancelor_common_model.Kind.base_of_yojson: not a JSON string"

let to_pretty_string ?(capitalised=false) base =
  (
    match base with
    | Jig -> "jig"
    | Polka -> "polka"
    | Reel -> "reel"
    | Strathspey -> "strathspey"
    | Waltz -> "waltz"
  )
  |> if capitalised then String.capitalize_ascii else Fun.id

let tempo = function
  | Jig -> ("4.", 108)
  | Polka | Reel -> ("2", 108)
  | Strathspey -> ("2", 60)
  | Waltz -> ("2.", 60)

type base_kind = t (* needed for the interface of filters *)

module Filter = struct
  type predicate =
    | Is of t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let is' = Formula.pred % is

  let accepts filter kind =
    Formula.interpret filter @@ function

    | Is kind' ->
      Lwt.return (Formula.interpret_bool (kind = kind'))

  let text_formula_converter =
    TextFormulaConverter.(
      make
        [
          raw
            (fun string ->
               Option.fold
                 ~some: (Result.ok % is')
                 ~none: (kspf Result.error "could not interpret \"%s\" as a base kind" string)
                 (of_string_opt string));
          unary_raw ~wrap_back:Never ~name:"is" (is, unIs) ~cast:(of_string_opt, to_pretty_string ~capitalised:true) ~type_:"base kind";
        ]
    )

  let from_text_formula = TextFormula.to_formula text_formula_converter

  let optimise = Formula.optimise @@ function
    | (Is _ as p) -> p
end
