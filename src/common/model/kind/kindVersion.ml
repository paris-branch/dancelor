open Nes

let _key = "kind-version"

type t = int * KindBase.t

let to_string (repeats, base) =
  spf "%d %s" repeats (KindBase.to_string base)

let of_string s =
  let s = NesString.remove_char ' ' s in
  try
    ssf s "%d%[a-zA-Z]"
      (fun repeats base -> (repeats, KindBase.of_string base))
  with
  | End_of_file | Scanf.Scan_failure _ ->
    try
      ssf s "%[a-zA-Z]%d"
        (fun base repeats -> (repeats, KindBase.of_string base))
    with
    | End_of_file | Scanf.Scan_failure _ ->
      invalid_arg "Dancelor_common_model.Kind.version_of_string"

let%test _ = to_string (32, Waltz) = "32 W"
let%test _ = to_string (64, Reel) = "64 R"
let%test _ = to_string (24, Jig) = "24 J"
let%test _ = to_string (48, Strathspey) = "48 S"

let%test _ = of_string "W 32" = (32, Waltz)
let%test _ = of_string "64 Reel" = (64, Reel)
let%test _ = of_string "JIG 24" = (24, Jig)
let%test _ = of_string "48 sTrathPEY" = (48, Strathspey)

let%test _ =
  try ignore (of_string "R"); false
  with Invalid_argument _ -> true
let%test _ =
  try ignore (of_string "8x32R"); false
  with Invalid_argument _ -> true

let of_string_opt string =
  try Some (of_string string)
  with Invalid_argument _ -> None

let to_yojson t =
  `String (to_string t)

let of_yojson = function
  | `String s ->
    (try Ok (of_string s)
     with _ -> Error "Dancelor_common_model.Kind.version_of_yojson: not a valid version kind")
  | _ -> Error "Dancelor_common_model.Kind.version_of_yojson: not a JSON string"

let to_pretty_string (repeats, base) =
  spf "%d %s" repeats (KindBase.to_pretty_string ~capitalised:true base)

(* Filters  *)

type version_kind = t (* needed for the inferface of filters *)

module Filter = struct
  type predicate =
    | Is of t
    | BarsEq of int | BarsGt of int | BarsLt of int
    | Base of KindBase.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is kind = Formula.pred (Is kind)
  let barsEq int = Formula.pred (BarsEq int)
  let barsNe int = Formula.not_ (barsEq int)
  let barsGt int = Formula.pred (BarsGt int)
  let barsGe int = Formula.or_l [ barsEq int; barsGt int ]
  let barsLt int = Formula.pred (BarsLt int)
  let barsLe int = Formula.or_l [ barsEq int; barsLt int ]

  let base bfilter = Formula.pred (Base bfilter)

  let accepts filter kind =
    Formula.interpret filter @@ function

    | Is kind' ->
      Lwt.return (Formula.interpret_bool (kind = kind'))

    | BarsEq bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars = bars'))

    | BarsGt bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars > bars'))

    | BarsLt bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars < bars'))

    | Base bfilter ->
      let (_bars, bkind) = kind in
      KindBase.Filter.accepts bfilter bkind

  let raw string =
    match KindBase.of_string_opt string with
    | Some bkind -> Ok (base (KindBase.Filter.is bkind))
    | None ->
      match of_string_opt string with
      | Some vkind -> Ok (is vkind)
      | None -> error_fmt "could not interpret \"%s\" as a kind for versions" string

  let nullary_text_predicates = []

  let unary_text_predicates =
    TextFormula.[
      unary ~name:"bars-eq" (raw_only ~convert:convert_int barsEq);
      unary ~name:"bars-ne" (raw_only ~convert:convert_int barsNe);
      unary ~name:"bars-gt" (raw_only ~convert:convert_int barsGt);
      unary ~name:"bars-ge" (raw_only ~convert:convert_int barsGe);
      unary ~name:"bars-lt" (raw_only ~convert:convert_int barsLt);
      unary ~name:"bars-le" (raw_only ~convert:convert_int barsLe);
    ]

  let from_text_formula =
    TextFormula.make_to_formula
      raw
      nullary_text_predicates
      unary_text_predicates
end
