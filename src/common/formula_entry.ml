(* FIXME: move as a sublibrary of Formula *)

open Nes

type ('v, 'f) predicate =
  | Is of 'v Entry.Id.t
  | Value of 'f
[@@deriving eq, show, yojson, variants]

type ('v, 'f) t = ('v, 'f) predicate Formula.t
[@@deriving eq, show, yojson]

let is' entry = Formula.pred @@ is @@ Entry.id entry
let value' f = Formula.pred @@ value f

let text_formula_converter sub_raw sub_tfc =
  Text_formula_converter.(
    make
      ~raw: (ok % value' % sub_raw)
      [
        unary_id ~name: "is" (is, is_val);
        unary_lift ~name: "value" (value, value_val) ~converter: sub_tfc ~wrap_back: Never;
      ]
  )

let from_text_formula sub_raw sub_tfc = Text_formula_converter.to_formula (text_formula_converter sub_raw sub_tfc)
let from_string sub_raw sub_tfc ?filename input =
  Result.bind (Text_formula.from_string ?filename input) (from_text_formula sub_raw sub_tfc)

let optimise sub_optimise =
  Formula.optimise
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Value f1, Value f2) -> some @@ value (op f1 f2)
      | _ -> None
    )
    (function
      | Is _ as p -> p
      | Value f -> value @@ sub_optimise f
    )

let accepts sub_accepts (filter : ('value, 'sub_filter) t) (entry : ('value, 'access) Entry.t) =
  Formula.interpret filter @@ function
    | Is id -> lwt @@ Formula.interpret_bool (Entry.Id.equal' id (Entry.id entry))
    | Value sub_filter -> sub_accepts sub_filter (Entry.value entry)

(* Allow building a Madge jsonable formula entry. Honestly, this is more like a
   hack at this point. FIXME by introducing a general module type for filters
   and making everything work nicely with modules. This is already something we
   need to make this text_formula_converter business less disgusting. *)

module type MODEL = sig type t end

module J (M : MODEL) (J : Madge.JSONABLE) : Madge.JSONABLE with type t = (M.t, J.t) t = struct
  (* Little hack to convince yojson that it can serialise a ('v, 'f) t when we
     never serialise 'v. *)
  module M = struct
    include M
    let to_yojson _ = assert false
    let of_yojson _ = assert false
  end
  type ('v, 'f) s = ('v, 'f) t [@@deriving yojson]
  type t = (M.t, J.t) s [@@deriving yojson]
end
