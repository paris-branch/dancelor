open Nes
open Common

module Gen = QCheck_generators

let to_string_no_exn ~name ~show ~gen ~to_string =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: show
        gen
        (fun f ->
          try
            ignore (to_string f); true
          with
            | exn ->
              Format.printf "%s" (Printexc.to_string exn);
              false
        )
    )

let to_string_from_string_roundtrip ~name ~show ~optimise ~to_string ~from_string ~gen ~equal =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: (fun f ->
          spf
            "Filter:\n\n  %s\n\nOptimised:\n\n  %s\n\nText formula:\n\n  %s\n\nOutput:\n\n  %s\n\nOptimised:\n\n  %s\n\n"
            (show f)
            (show (optimise f))
            (to_string f)
            (Result.fold (from_string @@ to_string f) ~ok: show ~error: (spf "Error: %s"))
            (Result.fold (from_string @@ to_string f) ~ok: (show % optimise) ~error: (spf "Error: %s"))
        )
        gen
        (fun f ->
          Result.equal
            ~ok: equal
            ~error: (=)
            (Result.map optimise (from_string (to_string f)))
            (Ok (optimise f))
        )
    )

let optimise_idempotent ~name ~gen ~show ~optimise ~equal =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: (fun f ->
          "Input:\n\n  " ^
          show f ^
          "\n\nOptimised:\n\n  " ^
          show (optimise f) ^
          "\n\nOptimised twice:\n\n  " ^
          show (optimise (optimise f)) ^
          "\n\n"
        )
        gen
        (fun f ->
          let f1 = optimise f in
          equal (optimise f1) f1
        )
    )

module type MODEL = sig
  type predicate
  type t = predicate Formula.t
  val equal : t -> t -> bool
  val show : t -> string
end

module type CONVERTER = sig
  type predicate
  val converter : predicate Text_formula_converter.t
end

module type MODEL_CONVERTER = sig
  type predicate
  include MODEL with type predicate := predicate
  include CONVERTER with type predicate := predicate
end

module type GEN = sig
  type predicate
  type t = predicate Gen.Formula.t
  val gen : t QCheck2.Gen.t
end

let to_string_no_exn' (type p)
    ~name
    (module M : MODEL_CONVERTER with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  to_string_no_exn ~name ~show: M.show ~gen: G.gen ~to_string: (Text_formula.formula_to_string M.converter)

let to_string_from_string_roundtrip' (type p)
    ~name
    (module M : MODEL_CONVERTER with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  to_string_from_string_roundtrip
    ~name
    ~show: M.show
    ~optimise: (Text_formula_converter.optimise M.converter)
    ~to_string: (Text_formula.formula_to_string M.converter)
    ~from_string: (Text_formula.string_to_formula M.converter)
    ~gen: G.gen
    ~equal: M.equal

let optimise_idempotent' (type p)
    ~name
    (module M : MODEL_CONVERTER with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  optimise_idempotent ~name ~gen: G.gen ~equal: M.equal ~optimise: (Text_formula_converter.optimise M.converter) ~show: M.show

module Formula_unit = struct
  type predicate = unit
  [@@deriving ord, show]
  type t = unit Formula.t
  [@@deriving eq, show]

  let gen = Gen.Formula.gen (QCheck2.Gen.pure ())

  let converter =
    Text_formula_converter.(
      make
        ~debug_name: "unit"
        ~debug_print: pp_predicate
        ~raw: (fun _ -> Error "no raw")
        []
        ~compare_predicate
    )
end

module Formula_int = struct
  type predicate = int
  [@@deriving ord, show]
  type t = int Formula.t
  [@@deriving eq, show]

  let gen = Gen.Formula.gen QCheck2.Gen.int

  let converter =
    Text_formula_converter.(
      make
        ~debug_name: "int"
        ~debug_print: pp_predicate
        ~raw: (fun string -> Option.fold ~none: (Error "invalid int") ~some: (ok % Formula.pred) (int_of_string_opt string))
        []
        ~compare_predicate
    )
end

let () =
  Alcotest.run
    "formulas"
    [
      (
        "to_string raises no exception",
        [to_string_no_exn ~name: "Text_formula" ~show: Text_formula.show ~to_string: Text_formula.to_string ~gen: Gen.Text_formula.gen;
        to_string_no_exn' ~name: "Source.Filter" (module Filter_builder.Core.Source) (module Gen.Filter.Source);
        to_string_no_exn' ~name: "Person.Filter" (module Filter_builder.Core.Person) (module Gen.Filter.Person);
        to_string_no_exn' ~name: "Dance.Filter" (module Filter_builder.Core.Dance) (module Gen.Filter.Dance);
        to_string_no_exn' ~name: "Tune.Filter" (module Filter_builder.Core.Tune) (module Gen.Filter.Tune);
        to_string_no_exn' ~name: "Version.Filter" (module Filter_builder.Core.Version) (module Gen.Filter.Version);
        to_string_no_exn' ~name: "Set.Filter" (module Filter_builder.Core.Set) (module Gen.Filter.Set);
        to_string_no_exn' ~name: "Book.Filter" (module Filter_builder.Core.Book) (module Gen.Filter.Book);
        to_string_no_exn' ~name: "Any.Filter" (module Filter_builder.Core.Any) (module Gen.Filter.Any);
        ]
      );
      (
        "optimise % from_string % to_string = optimise",
        [
        (* [to_string_from_string_roundtrip ~name: "Text_formula" ~show: Text_formula.show ~to_string: Text_formula.to_string ~from_string: Text_formula.from_string ~equal: Text_formula.equal ~gen: Gen.Text_formula.gen; *)
        to_string_from_string_roundtrip' ~name: "Source.Filter" (module Filter_builder.Core.Source) (module Gen.Filter.Source);
        to_string_from_string_roundtrip' ~name: "Person.Filter" (module Filter_builder.Core.Person) (module Gen.Filter.Person);
        to_string_from_string_roundtrip' ~name: "Dance.Filter" (module Filter_builder.Core.Dance) (module Gen.Filter.Dance);
        to_string_from_string_roundtrip' ~name: "Tune.Filter" (module Filter_builder.Core.Tune) (module Gen.Filter.Tune);
        to_string_from_string_roundtrip' ~name: "Version.Filter" (module Filter_builder.Core.Version) (module Gen.Filter.Version);
        to_string_from_string_roundtrip' ~name: "Set.Filter" (module Filter_builder.Core.Set) (module Gen.Filter.Set);
        to_string_from_string_roundtrip' ~name: "Book.Filter" (module Filter_builder.Core.Book) (module Gen.Filter.Book);
        to_string_from_string_roundtrip' ~name: "Any.Filter" (module Filter_builder.Core.Any) (module Gen.Filter.Any);
        ]
      );
      (
        "optimise is idempotent",
        [optimise_idempotent' ~name: "Formula (unit)" (module Formula_unit) (module Formula_unit);
        optimise_idempotent' ~name: "Formula (int)" (module Formula_int) (module Formula_int);
        optimise_idempotent' ~name: "Source.Filter" (module Filter_builder.Core.Source) (module Gen.Filter.Source);
        optimise_idempotent' ~name: "Person.Filter" (module Filter_builder.Core.Person) (module Gen.Filter.Person);
        optimise_idempotent' ~name: "Dance.Filter" (module Filter_builder.Core.Dance) (module Gen.Filter.Dance);
        optimise_idempotent' ~name: "Tune.Filter" (module Filter_builder.Core.Tune) (module Gen.Filter.Tune);
        optimise_idempotent' ~name: "Version.Filter" (module Filter_builder.Core.Version) (module Gen.Filter.Version);
        optimise_idempotent' ~name: "Set.Filter" (module Filter_builder.Core.Set) (module Gen.Filter.Set);
        optimise_idempotent' ~name: "Book.Filter" (module Filter_builder.Core.Book) (module Gen.Filter.Book);
        optimise_idempotent' ~name: "Any.Filter" (module Filter_builder.Core.Any) (module Gen.Filter.Any);
        ]
      );
    ]
