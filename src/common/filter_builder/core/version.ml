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
    make
      ~debug_name: "version"
      ~debug_print: pp_predicate
      ~raw: (ok % tune' % Formula_entry.value' % Tune.name' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "tune" (tune, tune_val) (Formula_entry.converter_public Tune.converter);
        lifter ~name: "sources" (sources, sources_val) (Formula_list.converter (Formula_entry.converter_public Source.converter));
      ]
      [
        unary_raw ~name: "key" (key, key_val) ~cast: (Music.Key.of_string_opt, Music.Key.to_string) ~type_: "key";
      ]
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
