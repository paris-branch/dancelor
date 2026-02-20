open Nes

type t =
  | Jig
  | Reel
  | Strathspey
  | Waltz
  | Polka
  | Other
[@@deriving eq, ord, show {with_path = false}]

(* NOTE: The order matters as it is used by eg. the tune editor. *)
let all = [Jig; Reel; Strathspey; Waltz; Polka; Other]

let to_short_string = function
  | Jig -> "J"
  | Reel -> "R"
  | Strathspey -> "S"
  | Waltz -> "W"
  | Polka -> "P"
  | Other -> "O"

let to_long_string ~capitalised base =
  (if capitalised then String.capitalize_ascii else Fun.id)
    (
      match base with
      | Jig -> "jig"
      | Reel -> "reel"
      | Strathspey -> "strathspey"
      | Waltz -> "waltz"
      | Polka -> "polka"
      | Other -> "other"
    )

let of_string s =
  match String.lowercase_ascii s with
  | "j" | "jig" -> Jig
  | "p" | "polka" -> Polka
  | "r" | "reel" -> Reel
  | "s" | "strathspey" -> Strathspey
  | "w" | "waltz" -> Waltz
  | "o" | "other" -> Other
  | _ -> invalid_arg "Common.Kind.Base.of_string"

let of_string_opt s =
  try
    Some (of_string s)
  with
    | Invalid_argument _ -> None

let to_yojson b =
  `String (to_long_string ~capitalised: false b)

let of_yojson = function
  | `String s ->
    (
      try
        Ok (of_string s)
      with
        | _ -> Error "Common.Kind.Base.of_yojson: not a valid base kind"
    )
  | _ -> Error "Common.Kind.Base.of_yojson: not a JSON string"

let tempo = function
  | Jig -> ("4.", 108)
  | Polka | Reel -> ("2", 108)
  | Strathspey -> ("2", 60)
  | Waltz -> ("2.", 60)
  | Other -> ("2", 108)

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
        lwt (Formula.interpret_bool (kind = kind'))

  let text_formula_converter =
    Text_formula_converter.(
      make
        [
          raw
            (fun string ->
              Option.fold
                ~some: (ok % is')
                ~none: (kspf error "could not interpret \"%s\" as a base kind" string)
                (of_string_opt string)
            );
          unary_raw ~wrap_back: Never ~name: "is" (is, is_val) ~cast: (of_string_opt, to_long_string ~capitalised: true) ~type_: "base kind";
        ]
    )

  let from_text_formula = Text_formula.to_formula text_formula_converter

  let optimise =
    Formula.optimise
      ~binop: (fun _ _ _ -> None)
      (function
        | (Is _ as p) -> p
      )
end
