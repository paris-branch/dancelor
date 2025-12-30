open Nes

module Access = Access
module Id = Id
module Meta = Meta
module User = User

type 'value id = 'value Id.t
type user = User.t
type user_id = user id

type 'value t = {
  id: 'value Id.t;
  value: 'value;
  meta: Meta.t;
  access: Access.t;
}
[@@deriving show {with_path = false}, fields, yojson]

let make' ~id ?meta ~access value =
  let meta = Option.value ~default: (Meta.make ()) meta in
    {id; meta; access; value}

let make ~id ?status ?privacy ?created_at ?modified_at ~owner value =
  make'
    ~id
    ~meta: (Meta.make ?status ?privacy ?created_at ?modified_at ())
    ~access: (Access.make ~owner)
    value

let id' e = Id.unsafe_coerce (id e)
let id_as_string e = Id.to_string (id e)

let equal' e f = Id.equal' (id e) (id f)
let equal _ = equal'
let compare' e f = Id.compare' (id e) (id f)
let compare _ = compare'

module No_id = struct
  type 'value t = {
    value: 'value;
    meta: Meta.t;
    access: Access.t;
  }
  [@@deriving yojson]
end

let to_yojson_no_id value_to_yojson entry =
  let {id = _; value; meta; access} = entry in
  No_id.to_yojson value_to_yojson No_id.{value; meta; access}

let of_yojson_no_id id value_of_yojson yojson =
  Result.map
    (fun No_id.{value; meta; access} -> {id; value; meta; access})
    (No_id.of_yojson value_of_yojson yojson)

module J (A : Madge.JSONABLE) : Madge.JSONABLE with type t = A.t t = struct
  type nonrec t = A.t t
  let to_yojson = to_yojson A.to_yojson
  let of_yojson = of_yojson A.of_yojson
end

let unsafe_set_value {id; meta; access; _} value =
  {id = Id.unsafe_coerce id; meta; access; value}
