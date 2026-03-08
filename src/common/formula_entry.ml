(* FIXME: move as a sublibrary of Formula *)

open Nes

type meta_predicate =
  | Newest
[@@deriving eq, show {with_path = false}, yojson, variants]

type meta = meta_predicate Formula.t
[@@deriving eq, show, yojson]

let optimise_meta =
  Formula.optimise
    (function
      | Newest as p -> p
    )

let accepts_meta filter meta =
  Formula.interpret filter @@ function
    | Newest ->
      (* Bit of a hack: we hardcode a notion of “all times”, and we map the
         creation date of the entry to [0., 1.] depending on how close it is to
         “all times” ago vs. how close it is to now. *)
      let all_times = float_of_int @@ 5 * 364 * 24 * 3600 in
      let reference = Datetime.make_in_the_past all_times in
      let x = Datetime.diff (Entry.Meta.created_at meta) reference in
      let x = if x < 0. then 0. else x in
      lwt (x /. all_times)

let meta_converter =
  Text_formula_converter.(
    make ~raw: (const @@ Error "meta does not accept raw converter") [
      nullary ~name: "newest" Newest;
    ]
  )

type ('value, 'filter, 'access_filter) predicate_gen =
  | Is of 'value Entry.Id.t
  | Value of 'filter
  | Access of 'access_filter
  | Meta of meta
[@@deriving eq, show {with_path = false}, yojson, variants]

type ('value, 'filter, 'access_filter) gen = ('value, 'filter, 'access_filter) predicate_gen Formula.t
[@@deriving eq, show, yojson]

type access_public_predicate =
  Public
[@@deriving eq, show, yojson, variants]

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
[@@deriving eq, show, yojson, variants]

type access_private = access_private_predicate Formula.t
[@@deriving eq, show, yojson]

type ('value, 'filter) predicate_private =
('value, 'filter, access_private) predicate_gen
[@@deriving eq, show, yojson]

type ('value, 'filter) private_ =
('value, 'filter, access_private) gen
[@@deriving eq, show, yojson]

let is' entry = Formula.pred @@ is @@ Entry.id entry
let value' filter = Formula.pred @@ value filter
let access' filter = Formula.pred @@ access filter
let meta' filter = Formula.pred @@ meta filter
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
            unary_lift ~name: "value" (value, value_val) ~converter: sub_converter ~wrap_back: Never;
            unary_lift ~name: "access" (access, access_val) ~converter: access_converter ~wrap_back: Never;
            unary_lift ~name: "meta" (meta, meta_val) ~converter: meta_converter ~wrap_back: Never;
          ]
      )
      (
        (* Sub converters, lifted to entries. They lose in case of tiebreak.
           NOTE: The access and meta converters win over the value
           sub-converter, without which we risk giving priority to access and
           meta of a sub-converter. For instance, [:public] for a version entry,
           needs to match that version's access, and not the tune's access. *)
        merge
          ~tiebreaker: Right
          (map value sub_converter ~error: ((^) "As entry value: "))
          (
            merge_l [
              map access access_converter ~error: ((^) "As entry access: ");
              map meta meta_converter ~error: ((^) "As entry meta: ");
            ]
          )
      )
  )

let converter_public sub_converter =
  converter_gen sub_converter (
    Text_formula_converter.(
      make ~raw: (const @@ Error "access does not accept raw converter") [
        nullary ~name: "public" Public;
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
    ~up: (fun {is_tf} ->
      function
        | Value f -> is_tf f
        | Access f -> is_tf f
        | Meta f -> is_tf f
        | _ -> false
    )
    ~not_: (function
      | Value f -> some @@ value' @@ Formula.not f
      | Access f -> some @@ access' @@ Formula.not f
      | Meta f -> some @@ meta' @@ Formula.not f
      | _ -> None
    )
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Value f1, Value f2) -> some @@ value (op f1 f2)
      | (Access f1, Access f2) -> some @@ access (op f1 f2)
      | (Meta f1, Meta f2) -> some @@ meta (op f1 f2)
      | _ -> None
    )
    (function
      | Is _ as p -> p
      | Value filter -> value @@ optimise_value filter
      | Access filter -> access @@ optimise_access filter
      | Meta filter -> meta @@ optimise_meta filter
    )

let optimise_public optimise_value =
  optimise_gen optimise_value @@
    Formula.optimise
      (function Public -> Public)

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
    | Meta filter -> accepts_meta filter (Entry.meta entry)

let accepts_public accepts_value (filter : ('value, 'filter) public) (entry : 'value Entry.public) =
  accepts_gen accepts_value (fun filter _access -> Formula.interpret filter (fun Public -> lwt Formula.interpret_true)) filter entry

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
