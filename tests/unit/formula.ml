open Common

module Gen = QCheckGenerators

let to_string_no_exn ~name ~show ~gen ~to_string =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: show
        gen
        (fun f -> try ignore (to_string f); true with _exn -> false)
    )

let to_string_from_string_roundtrip ~name ~show ~to_string ~from_string ~gen ~equal =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: (fun f ->
          "Filter:\n\n  " ^
          show f ^
          "\n\nText formula:\n\n  " ^
          to_string f ^
          "\n\nOutput:\n\n  " ^
          match from_string (to_string f) with
          | Ok f -> show f
          | Error err -> "Error: " ^ err
        )
        gen
        (fun f ->
          Result.equal
            ~ok: equal
            ~error: (=)
            (from_string (to_string f))
            (Ok f)
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

module type TOFROMSTRING = sig
  type predicate
  type t = predicate Formula.t
  val to_string : t -> string
  val from_string : ?filename: string -> string -> (t, string) result
end

module type OPTIMISE = sig
  type predicate
  type t = predicate Formula.t
  val optimise : t -> t
end

module type MODEL_TOFROMSTRING = sig
  type predicate
  include MODEL with type predicate := predicate
  include TOFROMSTRING with type predicate := predicate
end

module type MODEL_OPTIMISE = sig
  type predicate
  include MODEL with type predicate := predicate
  include OPTIMISE with type predicate := predicate
end

module type GEN = sig
  type predicate
  type t = predicate Gen.Formula.t
  val gen : t QCheck2.Gen.t
end

let to_string_no_exn' (type p)
    ~name
    (module M : MODEL_TOFROMSTRING with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  to_string_no_exn ~name ~show: M.show ~gen: G.gen ~to_string: M.to_string

let to_string_from_string_roundtrip' (type p)
    ~name
    (module M : MODEL_TOFROMSTRING with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  to_string_from_string_roundtrip
    ~name
    ~show: M.show
    ~to_string: M.to_string
    ~from_string: M.from_string
    ~gen: G.gen
    ~equal: M.equal

let optimise_idempotent' (type p)
    ~name
    (module M : MODEL_OPTIMISE with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  optimise_idempotent ~name ~gen: G.gen ~equal: M.equal ~optimise: M.optimise ~show: M.show

module FormulaUnit = struct
  type predicate = unit
  type t = unit Formula.t
  [@@deriving eq, show]

  let gen = Gen.Formula.gen (QCheck2.Gen.pure ())
  let optimise = Formula.optimise Fun.id
end

module FormulaInt = struct
  type predicate = int
  type t = int Formula.t
  [@@deriving eq, show]

  let gen = Gen.Formula.gen QCheck2.Gen.int

  let lift_and x y = Some (min x y)
  let lift_or x y = Some (max x y)
  let optimise =
    Formula.optimise ~lift_and ~lift_or @@ function
      | n when n mod 2 = 1 -> n - 1
      | n -> n
end

let () =
  Alcotest.run
    "formulas"
    [
      (
        "to_string raises no exception",
        [to_string_no_exn' ~name: "TextFormula" (module TextFormula) (module Gen.TextFormula);
        to_string_no_exn' ~name: "Source.Filter" (module ModelBuilder.Filter.Source) (module Gen.Source.Filter);
        to_string_no_exn' ~name: "Person.Filter" (module ModelBuilder.Filter.Person) (module Gen.Person.Filter);
        to_string_no_exn' ~name: "Dance.Filter" (module ModelBuilder.Filter.Dance) (module Gen.Dance.Filter);
        to_string_no_exn' ~name: "Tune.Filter" (module ModelBuilder.Filter.Tune) (module Gen.Tune.Filter);
        to_string_no_exn' ~name: "Version.Filter" (module ModelBuilder.Filter.Version) (module Gen.Version.Filter);
        to_string_no_exn' ~name: "Set.Filter" (module ModelBuilder.Filter.Set) (module Gen.Set.Filter);
        to_string_no_exn' ~name: "Book.Filter" (module ModelBuilder.Filter.Book) (module Gen.Book.Filter);
        to_string_no_exn' ~name: "Any.Filter" (module ModelBuilder.Filter.Any) (module Gen.Any.Filter);
        to_string_no_exn ~name: "Any.Filter (pretty)" ~gen: Gen.Any.Filter.gen ~show: ModelBuilder.Filter.Any.show ~to_string: ModelBuilder.Filter.Any.to_pretty_string;
        ]
      );
      (
        "from_string % to_string = id",
        [to_string_from_string_roundtrip' ~name: "TextFormula" (module TextFormula) (module Gen.TextFormula);
        to_string_from_string_roundtrip' ~name: "Source.Filter" (module ModelBuilder.Filter.Source) (module Gen.Source.Filter);
        to_string_from_string_roundtrip' ~name: "Person.Filter" (module ModelBuilder.Filter.Person) (module Gen.Person.Filter);
        to_string_from_string_roundtrip' ~name: "Dance.Filter" (module ModelBuilder.Filter.Dance) (module Gen.Dance.Filter);
        to_string_from_string_roundtrip' ~name: "Tune.Filter" (module ModelBuilder.Filter.Tune) (module Gen.Tune.Filter);
        to_string_from_string_roundtrip' ~name: "Version.Filter" (module ModelBuilder.Filter.Version) (module Gen.Version.Filter);
        to_string_from_string_roundtrip' ~name: "Set.Filter" (module ModelBuilder.Filter.Set) (module Gen.Set.Filter);
        to_string_from_string_roundtrip' ~name: "Book.Filter" (module ModelBuilder.Filter.Book) (module Gen.Book.Filter);
        to_string_from_string_roundtrip' ~name: "Any.Filter" (module ModelBuilder.Filter.Any) (module Gen.Any.Filter);
        (* FIXME: Does not actually hold. *)
        (* to_string_from_string_roundtrip  ~name:"Any.Filter (pretty)" *)
        (*   ~gen: (QCheck2.Gen.map Model.Any.Filter.optimise Gen.Any.Filter.gen) *)
        (*   ~show: Model.Any.Filter.show *)
        (*   ~to_string: Model.Any.Filter.to_pretty_string *)
        (*   ~from_string: Model.Any.Filter.(Result.map optimise % from_string) *)
        (*   ~equal: Model.Any.Filter.equal; *)
        ]
      );
      (
        "optimise is idempotent",
        [optimise_idempotent' ~name: "Formula (unit)" (module FormulaUnit) (module FormulaUnit);
        optimise_idempotent' ~name: "Formula (int)" (module FormulaInt) (module FormulaInt);
        optimise_idempotent' ~name: "Source.Filter" (module ModelBuilder.Filter.Source) (module Gen.Source.Filter);
        optimise_idempotent' ~name: "Person.Filter" (module ModelBuilder.Filter.Person) (module Gen.Person.Filter);
        optimise_idempotent' ~name: "Dance.Filter" (module ModelBuilder.Filter.Dance) (module Gen.Dance.Filter);
        optimise_idempotent' ~name: "Tune.Filter" (module ModelBuilder.Filter.Tune) (module Gen.Tune.Filter);
        optimise_idempotent' ~name: "Version.Filter" (module ModelBuilder.Filter.Version) (module Gen.Version.Filter);
        optimise_idempotent' ~name: "Set.Filter" (module ModelBuilder.Filter.Set) (module Gen.Set.Filter);
        optimise_idempotent' ~name: "Book.Filter" (module ModelBuilder.Filter.Book) (module Gen.Book.Filter);
        optimise_idempotent ~name: "Any.Filter (type_based_cleanup)" ~optimise: ModelBuilder.Filter.Any.type_based_cleanup ~gen: Gen.Any.Filter.gen ~show: ModelBuilder.Filter.Any.show ~equal: ModelBuilder.Filter.Any.equal;
        optimise_idempotent' ~name: "Any.Filter" (module ModelBuilder.Filter.Any) (module Gen.Any.Filter);
        ]
      );
    ]
