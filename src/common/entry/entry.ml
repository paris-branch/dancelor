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
  meta: Meta.t;
  access: Access.t;
  value: 'value
}
[@@deriving show {with_path = false}, fields]

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

let to_yojson' value_to_yojson {id = _; meta; access; value} =
  Json.merge_assoc_l [
    value_to_yojson value;
    Meta.to_yojson meta;
    Access.to_yojson access;
  ]

let to_yojson value_to_yojson entry =
  Json.add_field "id" (Id.to_yojson value_to_yojson entry.id) @@
    to_yojson' value_to_yojson entry

let of_yojson' id value_of_yojson json =
  let (status, json) = Option.value (Json.extract_field_opt "status" json) ~default: (Status.(to_yojson bot), json) in
  let (privacy, json) = Option.value (Json.extract_field_opt "privacy" json) ~default: (Privacy.(to_yojson default), json) in
  let (created_at, json) = Json.extract_field "created-at" json in
  let (modified_at, json) = Json.extract_field "modified-at" json in
  let meta_json = `Assoc [("status", status); ("privacy", privacy); ("created-at", created_at); ("modified-at", modified_at)] in
  let (owner, json) = Json.extract_field "owner" json in
  let access_json = `Assoc [("owner", owner)] in
  let value_json = json in
  Result.bind (Meta.of_yojson meta_json) @@ fun meta ->
  Result.bind (Access.of_yojson access_json) @@ fun access ->
  Result.bind (value_of_yojson value_json) @@ fun value ->
  Ok {id; meta; access; value}

let of_yojson value_of_yojson json =
  match Json.extract_field_opt "id" json with
  | Some (id, json) ->
    Result.bind (Id.of_yojson value_of_yojson id) @@ fun id ->
    of_yojson' id value_of_yojson json
  | None -> Result.error "missing id"

module J (A : Madge.JSONABLE) : Madge.JSONABLE with type t = A.t t = struct
  type nonrec t = A.t t
  let to_yojson = to_yojson A.to_yojson
  let of_yojson = of_yojson A.of_yojson
end

let unsafe_set_value {id; meta; access; _} value =
  {id = Id.unsafe_coerce id; meta; access; value}
