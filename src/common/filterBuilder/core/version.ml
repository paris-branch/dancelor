open Nes

type predicate =
  | Is of ModelBuilder.Core.Version.t Entry.Id.t
  | Tune of Tune.t
  | Key of Music.Key.t
  | ExistsSource of Source.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let tune' = Formula.pred % tune
let key' = Formula.pred % key
let existssource' = Formula.pred % existssource

let text_formula_converter =
  Text_formula_converter.(
    merge
      ~tiebreaker: Left
      (
        (* Version-specific converter. *)
        make
          [
            unary_lift ~wrap_back: Not_raw ~name: "tune" (tune, tune_val) ~converter: Tune.text_formula_converter;
            unary_raw ~name: "key" (key, key_val) ~cast: (Music.Key.of_string_opt, Music.Key.to_string) ~type_: "key";
            unary_id ~name: "is" (is, is_val);
            unary_lift ~name: "exists-source" (existssource, existssource_val) ~converter: Source.text_formula_converter;
          ]
      )
      (
        (* Tune converter, lifted to versions. Lose in case of tiebreak. *)
        map tune Tune.text_formula_converter ~error: ((^) "As tune lifted to version: ")
      )
  )

let from_text_formula = Text_formula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind
    (Text_formula.from_string ?filename input)
    from_text_formula

let to_text_formula = Text_formula.of_formula text_formula_converter
let to_string = Text_formula.to_string % to_text_formula

let is x = is @@ Entry.id x
let is' x = Formula.pred @@ is x

let tuneis x = tune @@ Tune.is' x
let tuneis' x = Formula.pred @@ tuneis x

let memsource x = existssource @@ Source.is' x
let memsource' x = Formula.pred @@ memsource x

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Tune f1, Tune f2) -> some @@ tune (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Key _ as p) -> p
      | Tune tfilter -> tune @@ Tune.optimise tfilter
      | ExistsSource sfilter -> existssource @@ Source.optimise sfilter
    )
