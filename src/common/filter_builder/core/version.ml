open Nes

type predicate =
  | Is of Model_builder.Core.Version.t Entry.Id.t
  | Tune of Tune.t
  | Key of Music.Key.t
  | Sources of Source.t Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let tune' = Formula.pred % tune
let key' = Formula.pred % key
let sources' = Formula.pred % sources

let converter =
  Text_formula_converter.(
    merge
      ~tiebreaker: Left
      (
        (* Version-specific converter. *)
        make
          ~raw: (ok % tune' % Tune.name' % Formula_string.matches')
          [
            unary_lift ~wrap_back: Not_raw ~name: "tune" (tune, tune_val) ~converter: Tune.converter;
            unary_raw ~name: "key" (key, key_val) ~cast: (Music.Key.of_string_opt, Music.Key.to_string) ~type_: "key";
            unary_id ~name: "is" (is, is_val);
            unary_lift ~name: "sources" (sources, sources_val) ~converter: (Formula_list.converter (Source.name' % Formula_string.matches') Source.converter);
          ]
      )
      (
        (* Tune converter, lifted to versions. Lose in case of tiebreak. *)
        map tune Tune.converter ~error: ((^) "As tune lifted to version: ")
      )
  )

let from_text_formula = Text_formula.to_formula converter
let from_string ?filename input =
  Result.bind
    (Text_formula.from_string ?filename input)
    from_text_formula

let to_text_formula = Text_formula.of_formula converter
let to_string = Text_formula.to_string % to_text_formula

let is x = is @@ Entry.id x
let is' x = Formula.pred @@ is x

let optimise =
  Formula.optimise
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Tune f1, Tune f2) -> some @@ tune (op f1 f2)
      | _ -> None
    )
    (function
      | (Is _ as p) | (Key _ as p) -> p
      | Tune tfilter -> tune @@ Tune.optimise tfilter
      | Sources sfilter -> sources @@ Formula_list.optimise Source.optimise sfilter
    )
