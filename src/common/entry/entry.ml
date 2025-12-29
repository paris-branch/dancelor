open Nes

module Id = Id
module User = User

type 'a id = 'a Id.t
type user = User.t
type user_id = user id

type meta = {
  status: Status.t; [@default Status.bot]
  privacy: Privacy.t; [@default Privacy.default]
  created_at: Datetime.t; [@key "created-at"]
  modified_at: Datetime.t; [@key "modified-at"]
}
[@@deriving yojson, show {with_path = false}, fields]

let make_meta ?(status = Status.bot) ?(privacy = Privacy.default) ?created_at ?modified_at () =
  let now = Datetime.now () in
  {
    status;
    privacy;
    created_at = Option.value ~default: now created_at;
    modified_at = Option.value ~default: now modified_at;
  }

let update_meta ?status ?privacy ?created_at ?modified_at meta = {
  status = Option.value ~default: meta.status status;
  privacy = Option.value ~default: meta.privacy privacy;
  created_at = Option.value ~default: meta.created_at created_at;
  modified_at = Option.value ~default: meta.modified_at modified_at;
}

type access = {
  owner: User.t Id.t;
}
[@@deriving make, show, fields, yojson]

type 'a t = {
  id: 'a Id.t;
  meta: meta;
  access: access;
  value: 'a
}
[@@deriving show {with_path = false}, fields]

let make' ~id ?meta ~access value =
  let meta = Option.value ~default: (make_meta ()) meta in
    {id; meta; access; value}

let make ~id ?status ?privacy ?created_at ?modified_at ~owner value =
  make'
    ~id
    ~meta: (make_meta ?status ?privacy ?created_at ?modified_at ())
    ~access: (make_access ~owner)
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
    meta_to_yojson meta;
    access_to_yojson access;
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
  Result.bind (meta_of_yojson meta_json) @@ fun meta ->
  Result.bind (access_of_yojson access_json) @@ fun access ->
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
