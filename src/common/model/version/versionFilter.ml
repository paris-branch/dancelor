open Nes

let _key = "version-filter"

type predicate =
  | Is of VersionCore.t
  | Tune of TuneFilter.t
  | Key of Music.key
  | Kind of KindFilter.Version.t
  | Broken
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is version = Formula.pred (Is version)
let tune tfilter = Formula.pred (Tune tfilter)
let tuneIs tune_ = tune (TuneFilter.is tune_)
let key key_ = Formula.pred (Key key_)
let kind kfilter = Formula.pred (Kind kfilter)
let broken = Formula.pred Broken

let raw string =
  Result.map tune @@ TuneFilter.raw string

let nullary_text_predicates = [
  "reel",       (kind KindFilter.(Version.base (Base.is Reel)));       (* alias for kind:reel       FIXME: make this clearer *)
  "jig",        (kind KindFilter.(Version.base (Base.is Jig)));        (* alias for kind:jig        FIXME: make this clearer *)
  "strathspey", (kind KindFilter.(Version.base (Base.is Strathspey))); (* alias for kind:strathspey FIXME: make this clearer *)
  "waltz",      (kind KindFilter.(Version.base (Base.is Waltz)));      (* alias for kind:waltz      FIXME: make this clearer *)
  "broken",      broken;
]

let unary_text_predicates =
  TextFormula.[
    "tune",    (tune @@@@ TuneFilter.from_text_formula);
    "key",     raw_only ~convert:(fun s -> match Music.key_of_string_opt s with Some k -> Ok k | None -> Error "not a valid key") key;
    "kind",    (kind @@@@ KindFilter.Version.from_text_formula);
  ]
  @ (List.map
       (fun (name, pred) ->
          (name, fun x ->
              match pred x with
              | Ok tfilter -> Ok (tune tfilter)
              | Error err -> Error err))
       TuneFilter.unary_text_predicates)

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates

let from_string ?filename input =
  from_text_formula (TextFormula.from_string ?filename input)
