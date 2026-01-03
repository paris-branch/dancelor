open Nes

module Access = Access
module Id = Id
module Meta = Meta
module User = User

type 'value id = 'value Id.t
[@@deriving eq, show, yojson]

type user = User.t
type user_id = user id

type ('value, 'access) t = {
  id: 'value Id.t;
  value: 'value;
  meta: Meta.t;
  access: 'access;
}
[@@deriving show {with_path = false}, fields, yojson]

let make ~id ?meta ~access value =
  let meta = Option.value ~default: (Meta.make ()) meta in
    {id; meta; access; value}

let id' e = Id.unsafe_coerce (id e)
let id_as_string e = Id.to_string (id e)

(* equality and comparison are simply based on the ids *)
let equal' e f = Id.equal' (id e) (id f)
let compare' e f = Id.compare' (id e) (id f)

(* the following are only here in order to be compatible with [@@deriving eq, ord] *)
let equal _ _ = equal'
let compare _ _ = compare'
let compare_public _ = compare'
let compare_private_ _ = compare'

module No_id = struct
  type ('value, 'access) t = {
    value: 'value;
    meta: Meta.t;
    access: 'access;
  }
  [@@deriving yojson]
end

let to_yojson_no_id value_to_yojson access_to_yojson entry =
  let {id = _; value; meta; access} = entry in
  No_id.to_yojson value_to_yojson access_to_yojson No_id.{value; meta; access}

let of_yojson_no_id id value_of_yojson access_of_yojson yojson =
  Result.map
    (fun No_id.{value; meta; access} -> {id; value; meta; access})
    (No_id.of_yojson value_of_yojson access_of_yojson yojson)

type 'value public = ('value, Access.public) t [@@deriving eq, show, yojson]
type 'value private_ = ('value, Access.Private.t) t [@@deriving eq, show, yojson]

let value_public : 'value public -> 'value = value
let value_private_ : 'value private_ -> 'value = value

module JPublic (A : Madge.JSONABLE) : Madge.JSONABLE with type t = A.t public = struct
  type t = A.t public
  let to_yojson = to_yojson A.to_yojson Access.public_to_yojson
  let of_yojson = of_yojson A.of_yojson Access.public_of_yojson
end

module JPrivate (A : Madge.JSONABLE) : Madge.JSONABLE with type t = A.t private_ = struct
  type t = A.t private_
  let to_yojson = to_yojson A.to_yojson Access.Private.to_yojson
  let of_yojson = of_yojson A.of_yojson Access.Private.of_yojson
end

let unsafe_erase_value {id; meta; access; _} =
  {id = Id.unsafe_coerce id; meta; access; value = ()}

let unsafe_erase_value_and_access {id; meta; _} =
  {id = Id.unsafe_coerce id; meta; access = (); value = ()}
