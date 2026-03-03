(* FIXME: move as a sublibrary of Formula *)

open Nes

type ('value, 'filter, 'access_filter) predicate_gen =
  | Is of 'value Entry.Id.t
  | Value of 'filter
  | Access of 'access_filter
[@@deriving eq, show, yojson]

type ('value, 'filter, 'access_filter) gen = ('value, 'filter, 'access_filter) predicate_gen Formula.t
[@@deriving eq, show, yojson]

type access_public_predicate = unit
[@@deriving eq, show, yojson]

type access_public = access_public_predicate Formula.t
[@@deriving eq, show, yojson]

type ('value, 'filter) predicate_public =
('value, 'filter, access_public) predicate_gen
[@@deriving eq, show, yojson]

type ('value, 'filter) public =
('value, 'filter, access_public) gen
[@@deriving eq, show, yojson]

type access_private_predicate =
  | Owners of (Entry.User.t, Formula_user.t) public Formula_list.t
[@@deriving eq, show, yojson]

type access_private = access_private_predicate Formula.t
[@@deriving eq, show, yojson]

type ('value, 'filter) predicate_private =
('value, 'filter, access_private) predicate_gen
[@@deriving eq, show, yojson]

type ('value, 'filter) private_ =
('value, 'filter, access_private) gen
[@@deriving eq, show, yojson]

let is x = Is x
let value x = Value x
let access x = Access x
let owners x = Owners x

let is_val = function Is x -> Some x | _ -> None
let value_val = function Value x -> Some x | _ -> None
let access_val = function Access x -> Some x | _ -> None
let owners_val = function Owners x -> Some x

let is' entry = Formula.pred @@ is @@ Entry.id entry
let value' filter = Formula.pred @@ value filter
let access' filter = Formula.pred @@ access filter
let owners' filter = Formula.pred @@ owners filter

let converter_gen sub_converter access_converter =
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

let converter_public sub_converter =
  converter_gen sub_converter (
    Text_formula_converter.(
      make ~raw: (const @@ Error "access does not accept raw converter") [
        nullary ~name: "unit" ()
      ]
    )
  )

let converter_private sub_converter =
  converter_gen sub_converter (
    Text_formula_converter.(
      make
        ~raw: (const @@ Error "access does not accept raw converter")
        [
          unary_lift ~name: "owners" (owners, owners_val) ~converter: (Formula_list.converter (converter_public Formula_user.converter));
        ]
    )
  )

let optimise_gen optimise_value optimise_access =
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

let optimise_public optimise_value = optimise_gen optimise_value (Formula.optimise (fun () -> ()))

let optimise_private optimise_value =
  optimise_gen optimise_value @@
    Formula.optimise
      ~binop: (fun {op} f1 f2 ->
        match (f1, f2) with
        | (Owners f1, Owners f2) -> some @@ owners (op f1 f2)
      )
      (function
        | Owners filter -> owners @@ Formula_list.optimise (optimise_public Formula_user.optimise) filter
      )

let accepts_gen
    (accepts_value : 'filter -> 'value -> float Lwt.t)
    (accepts_access : 'access_filter -> 'access -> float Lwt.t)
    (filter : ('value, 'filter, 'access_filter) gen)
    (entry : ('value, 'access) Entry.t)
  =
  Formula.interpret filter @@ function
    | Is id -> lwt @@ Formula.interpret_bool (Entry.Id.equal' id (Entry.id entry))
    | Value filter -> accepts_value filter (Entry.value entry)
    | Access filter -> accepts_access filter (Entry.access entry)

let accepts_public accepts_value (filter : ('value, 'filter) public) (entry : 'value Entry.public) =
  accepts_gen accepts_value (fun filter _access -> Formula.interpret filter (fun () -> lwt Formula.interpret_true)) filter entry

let accepts_private get_user accepts_value (filter : ('value, 'filter) private_) (entry : 'value Entry.private_) =
  let accepts_access_private filter access =
    Formula.interpret filter @@ function
      | Owners lfilter ->
        let owners = NEList.to_list @@ Entry.Access.Private.owners access in
        let%lwt owners = Lwt_list.map_p (Lwt.map Option.get % get_user) owners in
        Formula_list.accepts (accepts_public Formula_user.accepts) lfilter owners
  in
  accepts_gen accepts_value accepts_access_private filter entry

(* Allow building a Madge jsonable formula entry. Honestly, this is more like a
   hack at this point. FIXME by introducing a general module type for filters
   and making everything work nicely with modules. This is already something we
   need to make this converter business less disgusting. *)

module type VALUE = sig type t end

module JGen
    (Value : VALUE)
    (Filter : Madge.JSONABLE)
    (Access_filter : Madge.JSONABLE)
  : Madge.JSONABLE with
  type t = (Value.t, Filter.t, Access_filter.t) gen
= struct
  (* Little hack to convince yojson that it can serialise a ('v, 'f) t when we
     never serialise 'v. *)
  module Value = struct
    include Value
    let to_yojson _ = assert false
    let of_yojson _ = assert false
  end
  type t = (Value.t, Filter.t, Access_filter.t) gen [@@deriving yojson]
end

module JPublic (Value : VALUE) (Filter : Madge.JSONABLE) : Madge.JSONABLE with type t = (Value.t, Filter.t) public = JGen(Value)(Filter)(struct type t = access_public [@@deriving yojson] end)

module JPrivate (Value : VALUE) (Filter : Madge.JSONABLE) : Madge.JSONABLE with type t = (Value.t, Filter.t) private_ = JGen(Value)(Filter)(struct type t = access_private [@@deriving yojson] end)
