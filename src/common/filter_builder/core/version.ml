open Nes

type predicate =
  | Tune of (Model_builder.Core.Tune.t, Tune.t) Formula_entry.public
  | Key of Music.Key.t
  | Sources of (Model_builder.Core.Source.t, Source.t) Formula_entry.public Formula_list.t
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
          ~raw: (ok % tune' % Formula_entry.value' % Tune.name' % Formula_string.matches')
          [
            unary_lift ~wrap_back: Not_raw ~name: "tune" (tune, tune_val) ~converter: (Formula_entry.converter_public Tune.converter);
            unary_raw ~name: "key" (key, key_val) ~cast: (Music.Key.of_string_opt, Music.Key.to_string) ~type_: "key";
            unary_lift ~name: "sources" (sources, sources_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Source.converter));
          ]
      )
      (
        (* Tune converter, lifted to versions. Lose in case of tiebreak. *)
        map tune (Formula_entry.converter_public Tune.converter) ~error: ((^) "As tune lifted to version: ")
      )
  )

let optimise =
  Formula.optimise
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Tune f1, Tune f2) -> some @@ tune (op f1 f2)
      | _ -> None
    )
    (function
      | (Key _ as p) -> p
      | Tune tfilter -> tune @@ Formula_entry.optimise_public Tune.optimise tfilter
      | Sources sfilter -> sources @@ Formula_list.optimise (Formula_entry.optimise_public Source.optimise) sfilter
    )
