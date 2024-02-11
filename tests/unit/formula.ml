open Dancelor_server_model

module type MODEL = sig
  type predicate
  type t = predicate Formula.t
  val equal : t -> t -> bool
  val show : t -> string
  val gen : t QCheck.Gen.t
  val shrink' : t QCheck.Shrink.t
  val to_string : t -> string
  val from_string : ?filename:string -> string -> (t, string) result
end

let to_string_no_exn ~name (module M : MODEL) =
  QCheck_alcotest.to_alcotest
    (
      QCheck.Test.make ~name
        (QCheck.make ~print:M.show ~shrink:M.shrink' M.gen)
        (fun f -> try ignore (M.to_string f); true with _exn -> false)
    )

let to_string_from_string_roundtrip ~name (module M : MODEL) =
  QCheck_alcotest.to_alcotest
    (
      QCheck.Test.make ~name
        (
          QCheck.make
            ~print: (fun f -> "Filter:\n\n  " ^ M.show f
                              ^ "\n\nText formula:\n\n  " ^ M.to_string f
                              ^ "\n\nOutput:\n\n  " ^ match M.from_string (M.to_string f) with
                              | Ok f -> M.show f
                              | Error err -> "Error: " ^ err)
            ~shrink: M.shrink'
            M.gen
        )
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
          to_string_no_exn ~name:"TextFormula" (module TextFormula);
          to_string_no_exn ~name:"Person.Filter" (module Person.Filter);
          to_string_no_exn ~name:"Set.Filter" (module Set.Filter);
          to_string_no_exn ~name:"Book.Filter" (module Book.Filter);
          to_string_no_exn ~name:"Tune.Filter" (module Tune.Filter);
          to_string_no_exn ~name:"Version.Filter" (module Version.Filter);
          to_string_no_exn ~name:"Dance.Filter" (module Dance.Filter);
          to_string_no_exn ~name:"Any.Filter" (module Any.Filter);
        ]);

      ("from_string % to_string = id", [
          to_string_from_string_roundtrip ~name:"TextFormula" (module TextFormula);
          to_string_from_string_roundtrip ~name:"Person.Filter" (module Person.Filter);
          to_string_from_string_roundtrip ~name:"Set.Filter" (module Set.Filter);
          to_string_from_string_roundtrip ~name:"Book.Filter" (module Book.Filter);
          to_string_from_string_roundtrip ~name:"Tune.Filter" (module Tune.Filter);
          to_string_from_string_roundtrip ~name:"Version.Filter" (module Version.Filter);
          to_string_from_string_roundtrip ~name:"Dance.Filter" (module Dance.Filter);
          to_string_from_string_roundtrip ~name:"Any.Filter" (module Any.Filter);
        ]);
    ]
