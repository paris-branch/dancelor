open Nes

type predicate = unit
[@@deriving eq, ord, show, yojson]

type t = predicate Formula.t
[@@deriving eq, ord, show, yojson]

let converter : predicate Text_formula_converter.t =
  Text_formula_converter.(
    make
      ~debug_name: "public access"
      ~debug_print: (fun fmt _ -> fpf fmt "<opaque access>")
      ~raw: (const @@ Error "user does not accept raw converter")
      [nullary ~name: "unit" ();
      ]
      ~compare_predicate
  )
