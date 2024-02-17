module Model = Dancelor_server_model
module Gen = QCheckGenerators

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

module type MODELGEN = sig
  type predicate
  type t = predicate Model.Formula.t
  val equal : t -> t -> bool
  val show : t -> string
  val to_string : t -> string
  val from_string : ?filename:string -> string -> (t, string) result
  val gen : t QCheck2.Gen.t
end

module MakeModelGen
    (M : MODEL)
    (G : GEN with type predicate = M.predicate)
  : MODELGEN with type predicate = M.predicate
= struct
  include M
  include G
end

let to_string_no_exn ~name (module M : MODELGEN) =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print:M.show
        M.gen
        (fun f -> try ignore (M.to_string f); true with _exn -> false)
    )

let to_string_from_string_roundtrip ~name (module M : MODELGEN) =
  QCheck_alcotest.to_alcotest
    (
      QCheck2.Test.make
        ~count: 1_000
        ~long_factor: 100
        ~name
        ~print: (fun f -> "Filter:\n\n  " ^ M.show f
                          ^ "\n\nText formula:\n\n  " ^ M.to_string f
                          ^ "\n\nOutput:\n\n  " ^ match M.from_string (M.to_string f) with
                          | Ok f -> M.show f
                          | Error err -> "Error: " ^ err)
        M.gen
        (fun f ->
           Result.equal
             ~ok:M.equal
             ~error:(=)
             (M.from_string (M.to_string f))
             (Ok f)
        )
    )

let () =
  Alcotest.run
    "formulas"
    [
      ("to_string raises no exception", [
          to_string_no_exn ~name:"TextFormula" (module MakeModelGen(Model.TextFormula)(Gen.TextFormula));
          to_string_no_exn ~name:"Person.Filter" (module MakeModelGen(Model.Person.Filter)(Gen.Person.Filter));
          to_string_no_exn ~name:"Set.Filter" (module MakeModelGen(Model.Set.Filter)(Gen.Set.Filter));
          to_string_no_exn ~name:"Book.Filter" (module MakeModelGen(Model.Book.Filter)(Gen.Book.Filter));
          to_string_no_exn ~name:"Tune.Filter" (module MakeModelGen(Model.Tune.Filter)(Gen.Tune.Filter));
          to_string_no_exn ~name:"Version.Filter" (module MakeModelGen(Model.Version.Filter)(Gen.Version.Filter));
          to_string_no_exn ~name:"Dance.Filter" (module MakeModelGen(Model.Dance.Filter)(Gen.Dance.Filter));
          to_string_no_exn ~name:"Any.Filter" (module MakeModelGen(Model.Any.Filter)(Gen.Any.Filter));
        ]);

      ("from_string % to_string = id", [
          to_string_from_string_roundtrip ~name:"TextFormula" (module MakeModelGen(Model.TextFormula)(Gen.TextFormula));
          to_string_from_string_roundtrip ~name:"Person.Filter" (module MakeModelGen(Model.Person.Filter)(Gen.Person.Filter));
          to_string_from_string_roundtrip ~name:"Set.Filter" (module MakeModelGen(Model.Set.Filter)(Gen.Set.Filter));
          to_string_from_string_roundtrip ~name:"Book.Filter" (module MakeModelGen(Model.Book.Filter)(Gen.Book.Filter));
          to_string_from_string_roundtrip ~name:"Tune.Filter" (module MakeModelGen(Model.Tune.Filter)(Gen.Tune.Filter));
          to_string_from_string_roundtrip ~name:"Version.Filter" (module MakeModelGen(Model.Version.Filter)(Gen.Version.Filter));
          to_string_from_string_roundtrip ~name:"Dance.Filter" (module MakeModelGen(Model.Dance.Filter)(Gen.Dance.Filter));
          to_string_from_string_roundtrip ~name:"Any.Filter" (module MakeModelGen(Model.Any.Filter)(Gen.Any.Filter));
        ]);
    ]
