module Model = Dancelor_server_model
module Gen = QCheckGenerators

let to_string_no_exn ~name ~show ~gen ~to_string =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print:show
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
        ~print: (fun f -> "Filter:\n\n  " ^ show f
                          ^ "\n\nText formula:\n\n  " ^ to_string f
                          ^ "\n\nOutput:\n\n  " ^ match from_string (to_string f) with
                          | Ok f -> show f
                          | Error err -> "Error: " ^ err)
        gen
        (fun f ->
           Result.equal
             ~ok:equal
             ~error:(=)
             (from_string (to_string f))
             (Ok f)
        )
    )

module type MODEL = sig
  type predicate
  type t = predicate Model.Formula.t
  val equal : t -> t -> bool
  val show : t -> string
  val to_string : t -> string
  val from_string : ?filename:string -> string -> (t, string) result
end

module type GEN = sig
  type predicate
  type t = predicate Gen.Formula.t
  val gen : t QCheck2.Gen.t
end

let to_string_no_exn'
    (type p)
    ~name
    (module M : MODEL with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  to_string_no_exn ~name ~show:M.show ~gen:G.gen ~to_string:M.to_string

let to_string_from_string_roundtrip'
    (type p)
    ~name
    (module M : MODEL with type predicate = p)
    (module G : GEN with type predicate = p)
  =
  to_string_from_string_roundtrip ~name ~show:M.show ~to_string:M.to_string
    ~from_string:M.from_string ~gen:G.gen ~equal:M.equal

let () =
  Alcotest.run
    "formulas"
    [
      ("to_string raises no exception", [
          to_string_no_exn' ~name:"TextFormula" (module Model.TextFormula) (module Gen.TextFormula);
          to_string_no_exn' ~name:"Person.Filter" (module Model.Person.Filter) (module Gen.Person.Filter);
          to_string_no_exn' ~name:"Set.Filter" (module Model.Set.Filter) (module Gen.Set.Filter);
          to_string_no_exn' ~name:"Book.Filter" (module Model.Book.Filter) (module Gen.Book.Filter);
          to_string_no_exn' ~name:"Tune.Filter" (module Model.Tune.Filter) (module Gen.Tune.Filter);
          to_string_no_exn' ~name:"Version.Filter" (module Model.Version.Filter) (module Gen.Version.Filter);
          to_string_no_exn' ~name:"Dance.Filter" (module Model.Dance.Filter) (module Gen.Dance.Filter);
          to_string_no_exn' ~name:"Any.Filter" (module Model.Any.Filter) (module Gen.Any.Filter);
        ]);

      ("from_string % to_string = id", [
          to_string_from_string_roundtrip' ~name:"TextFormula" (module Model.TextFormula) (module Gen.TextFormula);
          to_string_from_string_roundtrip' ~name:"Person.Filter" (module Model.Person.Filter) (module Gen.Person.Filter);
          to_string_from_string_roundtrip' ~name:"Set.Filter" (module Model.Set.Filter) (module Gen.Set.Filter);
          to_string_from_string_roundtrip' ~name:"Book.Filter" (module Model.Book.Filter) (module Gen.Book.Filter);
          to_string_from_string_roundtrip' ~name:"Tune.Filter" (module Model.Tune.Filter) (module Gen.Tune.Filter);
          to_string_from_string_roundtrip' ~name:"Version.Filter" (module Model.Version.Filter) (module Gen.Version.Filter);
          to_string_from_string_roundtrip' ~name:"Dance.Filter" (module Model.Dance.Filter) (module Gen.Dance.Filter);
          to_string_from_string_roundtrip' ~name:"Any.Filter" (module Model.Any.Filter) (module Gen.Any.Filter);
        ]);
    ]
