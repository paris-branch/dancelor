open Nes

type t = int * Kind_base.t
[@@deriving eq, show {with_path = false}]

let to_string (repeats, base) =
  spf "%d %s" repeats (Kind_base.to_short_string base)

let of_string s =
  let (part1, part2) = Option.get @@ String.split_2_on_char ' ' s in
  try
    (int_of_string part1, Kind_base.of_string part2)
  with
    | _ -> (int_of_string part2, Kind_base.of_string part1)

let%test _ = to_string (32, Waltz) = "32 W"
let%test _ = to_string (64, Reel) = "64 R"
let%test _ = to_string (24, Jig) = "24 J"
let%test _ = to_string (48, Strathspey) = "48 S"

let%test _ = of_string "W 32" = (32, Waltz)
let%test _ = of_string "64 Reel" = (64, Reel)
let%test _ = of_string "JIG 24" = (24, Jig)
let%test _ = of_string "48 sTrathsPEY" = (48, Strathspey)

let%test _ =
  try
    ignore (of_string "R"); false
  with
    | Invalid_argument _ -> true
let%test _ =
  try
    ignore (of_string "8x32R"); false
  with
    | Invalid_argument _ -> true

let of_string_opt string =
  try
    Some (of_string string)
  with
    | Invalid_argument _ -> None

let to_yojson t =
  `String (to_string t)

let of_yojson = function
  | `String s ->
    (
      try
        Ok (of_string s)
      with
        | _ -> Error "Dancelor_common.Model.Kind.version_of_yojson: not a valid version kind"
    )
  | _ -> Error "Dancelor_common.Model.Kind.version_of_yojson: not a JSON string"

let to_pretty_string (repeats, base) =
  spf "%d %s" repeats (Kind_base.to_long_string ~capitalised: true base)

(* Filters  *)

type version_kind = t (* needed for the inferface of filters *)

module Filter = struct
  type predicate =
    | Is of t
    | Bars_eq of int
    | Bars_ne of int
    | Bars_gt of int
    | Bars_ge of int
    | Bars_lt of int
    | Bars_le of int
    | Base of Kind_base.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  let base_is = base % Kind_base.Filter.is'

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let is' = Formula.pred % is
  let base' = Formula.pred % base

  let base_is' = Formula.pred % base_is

  let accepts filter kind =
    Formula.interpret filter @@ function
      | Is kind' ->
        lwt (Formula.interpret_bool (kind = kind'))
      | Bars_eq bars' ->
        let (bars, _) = kind in
        lwt (Formula.interpret_bool (bars = bars'))
      | Bars_ne bars' ->
        let (bars, _) = kind in
        lwt (Formula.interpret_bool (bars <> bars'))
      | Bars_gt bars' ->
        let (bars, _) = kind in
        lwt (Formula.interpret_bool (bars > bars'))
      | Bars_ge bars' ->
        let (bars, _) = kind in
        lwt (Formula.interpret_bool (bars >= bars'))
      | Bars_lt bars' ->
        let (bars, _) = kind in
        lwt (Formula.interpret_bool (bars < bars'))
      | Bars_le bars' ->
        let (bars, _) = kind in
        lwt (Formula.interpret_bool (bars <= bars'))
      | Base bfilter ->
        let (_bars, bkind) = kind in
        Kind_base.Filter.accepts bfilter bkind

  let text_formula_converter =
    Text_formula_converter.(
      merge
        (
          (* Version kind-specific converter *)
          make
            ~raw: (fun string ->
              Option.fold
                ~some: (ok % is')
                ~none: (kspf error "could not interpret \"%s\" as a version kind" string)
                (of_string_opt string)
            )
            [
              unary_int ~name: "bars-eq" (bars_eq, bars_eq_val);
              unary_int ~name: "bars-ne" (bars_ne, bars_ne_val);
              unary_int ~name: "bars-gt" (bars_gt, bars_gt_val);
              unary_int ~name: "bars-ge" (bars_ge, bars_ge_val);
              unary_int ~name: "bars-lt" (bars_lt, bars_lt_val);
              unary_int ~name: "bars-le" (bars_le, bars_le_val);
              unary_raw ~wrap_back: Never ~name: "is" (is, is_val) ~cast: (of_string_opt, to_pretty_string) ~type_: "version kind";
              unary_lift ~wrap_back: Not_pred ~name: "base" (base, base_val) ~converter: Kind_base.Filter.text_formula_converter;
            ]
        )
        (
          (* Base kind converter, lifted to version kinds *)
          map base Kind_base.Filter.text_formula_converter
        )
    )

  let from_text_formula = Text_formula.to_formula text_formula_converter
  let from_string ?filename input =
    Result.bind (Text_formula.from_string ?filename input) from_text_formula

  let optimise =
    Formula.optimise
      ~binop: (fun {op} f1 f2 ->
        match (f1, f2) with
        | (Base f1, Base f2) -> some @@ base (op f1 f2)
        | _ -> None
      )
      (function
        | (Is _ as p)
        | (Bars_eq _ as p)
        | (Bars_ne _ as p)
        | (Bars_gt _ as p)
        | (Bars_ge _ as p)
        | (Bars_lt _ as p)
        | (Bars_le _ as p) ->
          p
        | Base bfilter -> base @@ Kind_base.Filter.optimise bfilter
      )
end
