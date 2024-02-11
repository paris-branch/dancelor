open Nes

let _key = "kind-version"

type t = int * KindBase.t
[@@deriving eq, show {with_path = false}]

let gen = QCheck.Gen.(pair nat KindBase.gen)
let shrink _ = QCheck.Iter.empty

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
    | BarsEq of int
    | BarsNe of int
    | BarsGt of int
    | BarsGe of int
    | BarsLt of int
    | BarsLe of int
    | Base of KindBase.Filter.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  (* FIXME: PPX *)
  let is kind = Is kind
  let barsEq int = BarsEq int
  let barsNe int = BarsNe int
  let barsGt int = BarsGt int
  let barsGe int = BarsGe int
  let barsLt int = BarsLt int
  let barsLe int = BarsLe int
  let base bfilter = Base bfilter

  let unIs = function Is k -> Some k | _ -> None
  let unBarsEq = function BarsEq i -> Some i | _ -> None
  let unBarsNe = function BarsNe i -> Some i | _ -> None
  let unBarsGt = function BarsGt i -> Some i | _ -> None
  let unBarsGe = function BarsGe i -> Some i | _ -> None
  let unBarsLt = function BarsLt i -> Some i | _ -> None
  let unBarsLe = function BarsLe i -> Some i | _ -> None
  let unBase = function Base bf -> Some bf | _ -> None

  let shrink = let open QCheck.Iter in function
      | Is k -> map is (shrink k)
      | BarsEq n -> map barsEq (QCheck.Shrink.int n)
      | BarsNe n -> map barsNe (QCheck.Shrink.int n)
      | BarsGt n -> map barsGt (QCheck.Shrink.int n)
      | BarsGe n -> map barsGe (QCheck.Shrink.int n)
      | BarsLt n -> map barsLt (QCheck.Shrink.int n)
      | BarsLe n -> map barsLe (QCheck.Shrink.int n)
      | Base bf -> map base (KindBase.Filter.shrink' bf)

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, qcheck, yojson]

  let is' = Formula.pred % is
  let barsEq' = Formula.pred % barsEq
  let base' = Formula.pred % base

  let shrink' = Formula.shrink shrink

  let accepts filter kind =
    Formula.interpret filter @@ function

    | Is kind' ->
      Lwt.return (Formula.interpret_bool (kind = kind'))

    | BarsEq bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars = bars'))

    | BarsNe bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars <> bars'))

    | BarsGt bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars > bars'))

    | BarsGe bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars >= bars'))

    | BarsLt bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars < bars'))

    | BarsLe bars' ->
      let (bars, _) = kind in
      Lwt.return (Formula.interpret_bool (bars <= bars'))

    | Base bfilter ->
      let (_bars, bkind) = kind in
      KindBase.Filter.accepts bfilter bkind

  let text_formula_converter =
    TextFormulaConverter.(
      merge
        (
          (* Version kind-specific converter *)
          make
            [
              raw
                (fun string ->
                   Option.fold
                     ~some: (Result.ok % is')
                     ~none: (kspf Result.error "could not interpret \"%s\" as a version kind" string)
                     (of_string_opt string)
                );
              unary_int ~name:"bars-eq" (barsEq, unBarsEq);
              unary_int ~name:"bars-ne" (barsNe, unBarsNe);
              unary_int ~name:"bars-gt" (barsGt, unBarsGt);
              unary_int ~name:"bars-ge" (barsGe, unBarsGe);
              unary_int ~name:"bars-lt" (barsLt, unBarsLt);
              unary_int ~name:"bars-le" (barsLe, unBarsLe);
              unary_raw ~name:"is" (is, unIs) ~cast:(of_string_opt, to_pretty_string) ~type_:"version kind";
              unary_lift ~name:"base" (base, unBase) ~converter:KindBase.Filter.text_formula_converter;
            ]
        )
        (
          (* Base kind converter, lifted to version kinds *)
          map base KindBase.Filter.text_formula_converter
        )
    )

  let from_text_formula = TextFormula.to_formula text_formula_converter
  let from_string ?filename input =
    Result.bind (TextFormula.from_string ?filename input) from_text_formula
end
