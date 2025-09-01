open Nes

module Id = Id
module Slug = ESlug

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

type 'a t =
  {id: 'a Id.t; meta: meta; value: 'a}
[@@deriving show {with_path = false}, fields]

let make' ~id ?meta value =
  let meta = Option.value ~default: (make_meta ()) meta in
    {id; meta; value}

let make ~id ?status ?privacy ?created_at ?modified_at value =
  make' ~id ~meta: (make_meta ?status ?privacy ?created_at ?modified_at ()) value

let id' e = Id.unsafe_coerce (id e)
let id_as_string e = Id.to_string (id e)

let status' e = status @@ meta e
let privacy' e = privacy @@ meta e

let equal' e f = Id.equal' (id e) (id f)
let equal _ = equal'
let compare' e f = Id.compare' (id e) (id f)
let compare _ = compare'

let to_yojson value_to_yojson {id; meta; value} =
  Json.add_field "id" (Id.to_yojson value_to_yojson id) @@
    Json.merge_assoc (value_to_yojson value) (meta_to_yojson meta)

let to_yojson' value_to_yojson {meta; value; _} =
  Json.merge_assoc (value_to_yojson value) (meta_to_yojson meta)

let of_yojson' id value_of_yojson json =
  let (status, json) = Option.value (Json.extract_field_opt "status" json) ~default: (Status.(to_yojson bot), json) in
  let (privacy, json) = Option.value (Json.extract_field_opt "privacy" json) ~default: (Privacy.(to_yojson default), json) in
  let (created_at, json) = Json.extract_field "created-at" json in
  let (modified_at, value_json) = Json.extract_field "modified-at" json in
  let meta_json = `Assoc [("status", status); ("privacy", privacy); ("created-at", created_at); ("modified-at", modified_at)] in
  Result.bind (meta_of_yojson meta_json) @@ fun meta ->
  Result.bind (value_of_yojson value_json) @@ fun value ->
  Ok {id; meta; value}

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

let unsafe_set_value {id; meta; _} value =
  {id = Id.unsafe_coerce id; meta; value}
