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

(** Whether [optimise % from_string % to_string = optimise], up to optimisation. *)
let optimise_from_string_to_string__eq__optimise ~name ~show ~to_string ~from_string ~gen ~optimise ~equal =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: (fun f ->
          "Filter:\n\n  " ^
          show f ^
          "\n\nOptimised:\n\n  " ^
          show (optimise f) ^
          "\n\nText formula:\n\n  " ^
          to_string f ^
          "\n\nOutput:\n\n  " ^
          match from_string (to_string f) with
          | Ok f -> show f ^ "\n\nOptimised:\n\n  " ^ show (optimise f)
          | Error err -> "Error: " ^ err
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

(** Whether [from_string % to_string % optimise = optimise]. *)
let from_string_to_string_optimise__eq__optimise ~name ~show ~to_string ~from_string ~gen ~optimise ~equal =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: (fun f ->
          "Filter:\n\n  " ^
          show f ^
          "\n\nOptimised:\n\n  " ^
          show (optimise f) ^
          "\n\nText formula:\n\n  " ^
          to_string (optimise f) ^
          "\n\nOutput:\n\n  " ^
          match from_string (to_string (optimise f)) with
          | Ok f -> show f
          | Error err -> "Error: " ^ err
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
          "\n\nOptimised twice:\n\n  " ^ show (optimise (optimise f))
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

module type OPTIMISE = sig
  type predicate
  type t = predicate Formula.t
  val optimise : t -> t
end

module type MODEL_OPTIMISE = sig
  type predicate
  include MODEL with type predicate := predicate
  include OPTIMISE with type predicate := predicate
end

module type MODEL_CONVERTER = sig
  type predicate
  include MODEL with type predicate := predicate
  include OPTIMISE with type predicate := predicate
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

let optimise_from_string_to_string__eq__optimise' (type p)
    ~name
    (module M : MODEL_CONVERTER with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  optimise_from_string_to_string__eq__optimise
    ~name
    ~show: M.show
    ~to_string: (Text_formula.formula_to_string M.converter)
    ~from_string: (Text_formula.string_to_formula M.converter)
    ~gen: G.gen
    ~optimise: M.optimise
    ~equal: M.equal

let from_string_to_string_optimise__eq__optimise' (type p)
    ~name
    (module M : MODEL_CONVERTER with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  from_string_to_string_optimise__eq__optimise
    ~name
    ~show: M.show
    ~to_string: (Text_formula.formula_to_string M.converter)
    ~from_string: (Text_formula.string_to_formula M.converter)
    ~gen: G.gen
    ~optimise: M.optimise
    ~equal: M.equal

let optimise_idempotent' (type p)
    ~name
    (module M : MODEL_OPTIMISE with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  optimise_idempotent ~name ~gen: G.gen ~equal: M.equal ~optimise: M.optimise ~show: M.show

module Formula_unit = struct
  type predicate = unit
  type t = unit Formula.t
  [@@deriving eq, show]

  let gen = Gen.Formula.gen (QCheck2.Gen.pure ())
  let optimise = Formula.optimise Fun.id
end

module Formula_int = struct
  type predicate = int
  type t = int Formula.t
  [@@deriving eq, show]

  let converter =
    Text_formula_converter.(
      make ~raw: (fun _ -> Error "no raw") [
        unary_int ~name: "is" (Fun.id, Option.some);
      ]
    )

  let optimise =
    Formula.optimise ~and_: (fun x y -> Some (min x y)) ~or_: (fun x y -> Some (max x y)) (function
      | n when n mod 2 = 1 -> n - 1
      | n -> n
    )

  let gen = Gen.Formula.gen QCheck2.Gen.int
end

module Formula_list_int = struct
  type predicate = Formula_int.t Formula_list.predicate [@@deriving eq, show]
  type t = predicate Formula.t [@@deriving eq, show]
  let converter = Formula_list.converter Formula_int.converter
  let optimise = Formula_list.optimise Formula_int.optimise
  let gen : t QCheck2.Gen.t = Gen.Formula_list.gen Formula_int.gen
end

module Formula_entry_int = struct
  type predicate = (int, Formula_int.t) Formula_entry.predicate_public [@@deriving eq, show]
  type t = predicate Formula.t [@@deriving eq, show]
  let converter = Formula_entry.converter_public Formula_int.converter
  let optimise = Formula_entry.optimise_public Formula_int.optimise
  let gen : t QCheck2.Gen.t = Gen.Formula_entry.gen_public QCheck2.Gen.int Formula_int.gen
end

let () =
  Alcotest.run
    "formulas"
    [
      (
        "to_string raises no exception",
        [to_string_no_exn ~name: "Text_formula" ~show: Text_formula.show ~to_string: Text_formula.to_string ~gen: Gen.Text_formula.gen;
        to_string_no_exn' ~name: "Formula_list(int)" (module Formula_list_int) (module Formula_list_int);
        to_string_no_exn' ~name: "Formula_entry(int)" (module Formula_entry_int) (module Formula_entry_int);
        to_string_no_exn' ~name: "Source.Filter" (module Filter_builder.Core.Source) (module Gen.Filter.Source);
        to_string_no_exn' ~name: "Person.Filter" (module Filter_builder.Core.Person) (module Gen.Filter.Person);
        to_string_no_exn' ~name: "Dance.Filter" (module Filter_builder.Core.Dance) (module Gen.Filter.Dance);
        to_string_no_exn' ~name: "Tune.Filter" (module Filter_builder.Core.Tune) (module Gen.Filter.Tune);
        to_string_no_exn' ~name: "Version.Filter" (module Filter_builder.Core.Version) (module Gen.Filter.Version);
        to_string_no_exn' ~name: "Set.Filter" (module Filter_builder.Core.Set) (module Gen.Filter.Set);
        to_string_no_exn' ~name: "Book.Filter" (module Filter_builder.Core.Book) (module Gen.Filter.Book);
        to_string_no_exn' ~name: "Any.Filter" (module Filter_builder.Core.Any) (module Gen.Filter.Any);
        to_string_no_exn ~name: "Any.Filter (pretty)" ~gen: Gen.Filter.Any.gen ~show: Filter_builder.Core.Any.show ~to_string: Filter_builder.Core.Any.to_pretty_string;
        ]
      );
      (
        "optimise % from_string % to_string = optimise",
        [optimise_from_string_to_string__eq__optimise ~name: "Text_formula" ~show: Text_formula.show ~to_string: Text_formula.to_string ~from_string: Text_formula.from_string ~equal: Text_formula.equal ~gen: Gen.Text_formula.gen ~optimise: Fun.id;
        optimise_from_string_to_string__eq__optimise' ~name: "Source.Filter" (module Filter_builder.Core.Source) (module Gen.Filter.Source);
        optimise_from_string_to_string__eq__optimise' ~name: "Person.Filter" (module Filter_builder.Core.Person) (module Gen.Filter.Person);
        optimise_from_string_to_string__eq__optimise' ~name: "Dance.Filter" (module Filter_builder.Core.Dance) (module Gen.Filter.Dance);
        optimise_from_string_to_string__eq__optimise' ~name: "Tune.Filter" (module Filter_builder.Core.Tune) (module Gen.Filter.Tune);
        optimise_from_string_to_string__eq__optimise' ~name: "Version.Filter" (module Filter_builder.Core.Version) (module Gen.Filter.Version);
        optimise_from_string_to_string__eq__optimise' ~name: "Set.Filter" (module Filter_builder.Core.Set) (module Gen.Filter.Set);
        optimise_from_string_to_string__eq__optimise' ~name: "Book.Filter" (module Filter_builder.Core.Book) (module Gen.Filter.Book);
        optimise_from_string_to_string__eq__optimise' ~name: "Any.Filter" (module Filter_builder.Core.Any) (module Gen.Filter.Any);
        ]
      );
      (
        "from_string % to_string % optimise = optimise",
        [from_string_to_string_optimise__eq__optimise ~name: "Text_formula" ~show: Text_formula.show ~to_string: Text_formula.to_string ~from_string: Text_formula.from_string ~equal: Text_formula.equal ~gen: Gen.Text_formula.gen ~optimise: Fun.id;
        from_string_to_string_optimise__eq__optimise' ~name: "Source.Filter" (module Filter_builder.Core.Source) (module Gen.Filter.Source);
        from_string_to_string_optimise__eq__optimise' ~name: "Person.Filter" (module Filter_builder.Core.Person) (module Gen.Filter.Person);
        from_string_to_string_optimise__eq__optimise' ~name: "Dance.Filter" (module Filter_builder.Core.Dance) (module Gen.Filter.Dance);
        from_string_to_string_optimise__eq__optimise' ~name: "Tune.Filter" (module Filter_builder.Core.Tune) (module Gen.Filter.Tune);
        from_string_to_string_optimise__eq__optimise' ~name: "Version.Filter" (module Filter_builder.Core.Version) (module Gen.Filter.Version);
        from_string_to_string_optimise__eq__optimise' ~name: "Set.Filter" (module Filter_builder.Core.Set) (module Gen.Filter.Set);
        from_string_to_string_optimise__eq__optimise' ~name: "Book.Filter" (module Filter_builder.Core.Book) (module Gen.Filter.Book);
        from_string_to_string_optimise__eq__optimise' ~name: "Any.Filter" (module Filter_builder.Core.Any) (module Gen.Filter.Any);
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
        optimise_idempotent ~name: "Any.Filter (type_based_cleanup)" ~optimise: Filter_builder.Core.Any.type_based_cleanup ~gen: Gen.Filter.Any.gen ~show: Filter_builder.Core.Any.show ~equal: Filter_builder.Core.Any.equal;
        optimise_idempotent' ~name: "Any.Filter" (module Filter_builder.Core.Any) (module Gen.Filter.Any);
        ]
      );
    ]
