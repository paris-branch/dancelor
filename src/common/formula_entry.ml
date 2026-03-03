(* FIXME: move as a sublibrary of Formula *)

open Nes

module Access = struct
  module Public = struct
    type predicate = unit [@@deriving eq, show, yojson]
    type t = predicate Formula.t [@@deriving eq, show, yojson]
    let converter : predicate Text_formula_converter.t =
      Text_formula_converter.(
        make ~raw: (const @@ Error "access does not accept raw converter") [
          nullary ~name: "unit" ()
        ]
      )
    let optimise (filter : t) = Formula.optimise (fun () -> ()) filter
    let accepts (filter : t) (_access : Entry.Access.public) =
      Formula.interpret filter (fun () -> lwt Formula.interpret_true)
  end
end

type ('value, 'filter, 'access_filter) predicate =
  | Is of 'value Entry.Id.t
  | Value of 'filter
  | Access of 'access_filter
[@@deriving eq, show, yojson, variants]

type ('value, 'filter, 'access_filter) t = ('value, 'filter, 'access_filter) predicate Formula.t
[@@deriving eq, show, yojson]

type ('value, 'filter) predicate_public =
('value, 'filter, Access.Public.t) predicate
[@@deriving eq, show, yojson]

type ('value, 'filter) public =
('value, 'filter, Access.Public.t) t
[@@deriving eq, show, yojson]

let is' entry = Formula.pred @@ is @@ Entry.id entry
let value' filter = Formula.pred @@ value filter
let access' filter = Formula.pred @@ access filter

let converter sub_converter access_converter =
  Text_formula_converter.(
    merge
      ~tiebreaker: Left
      (
        make
          ~raw: (Result.map value' % raw sub_converter)
          [
            unary_id ~name: "is" (is, is_val);
            unary_lift ~name: "value" (value, value_val) ~converter: sub_converter;
            (* FIXME: should we use ~wrap_back: Never, for nicer text formulas? but
               this breaks the roundtrip tests and it isn't bothering use for now (but
               we've only applied this to Person, which has like no predicates). *)
            unary_lift ~name: "access" (access, access_val) ~converter: access_converter;
          ]
      )
      (
        (* Sub converters, lifted to entries. Lose in case of tiebreak. *)
        merge_l [
          map value sub_converter ~error: ((^) "As entry value: ");
          map access access_converter ~error: ((^) "As entry access: ");
        ]
      )
  )

let converter_public sub_converter = converter sub_converter Access.Public.converter

let optimise optimise_value optimise_access =
  Formula.optimise
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Value f1, Value f2) -> some @@ value (op f1 f2)
      | _ -> None
    )
    (function
      | Is _ as p -> p
      | Value filter -> value @@ optimise_value filter
      | Access filter -> access @@ optimise_access filter
    )

let optimise_public optimise_value = optimise optimise_value Access.Public.optimise

let accepts accepts_value accepts_access (filter : ('value, 'filter, 'access_filter) t) (entry : ('value, 'access) Entry.t) =
  Formula.interpret filter @@ function
    | Is id -> lwt @@ Formula.interpret_bool (Entry.Id.equal' id (Entry.id entry))
    | Value filter -> accepts_value filter (Entry.value entry)
    | Access filter -> accepts_access filter (Entry.access entry)

let accepts_public accepts_value = accepts accepts_value Access.Public.accepts

(* Allow building a Madge jsonable formula entry. Honestly, this is more like a
   hack at this point. FIXME by introducing a general module type for filters
   and making everything work nicely with modules. This is already something we
   need to make this converter business less disgusting. *)

module type VALUE = sig type t end

module J
    (Value : VALUE)
    (Filter : Madge.JSONABLE)
    (Access_filter : Madge.JSONABLE)
  : Madge.JSONABLE with
  type t = (Value.t, Filter.t, Access_filter.t) t
= struct
  (* Little hack to convince yojson that it can serialise a ('v, 'f) t when we
     never serialise 'v. *)
  module Value = struct
    include Value
    let to_yojson _ = assert false
    let of_yojson _ = assert false
  end
  type ('value, 'filter, 'access_filter) s = ('value, 'filter, 'access_filter) t [@@deriving yojson]
  type t = (Value.t, Filter.t, Access_filter.t) s [@@deriving yojson]
end

module JPublic (Value : VALUE) (Filter : Madge.JSONABLE) : Madge.JSONABLE with type t = (Value.t, Filter.t) public = J(Value)(Filter)(Access.Public)
