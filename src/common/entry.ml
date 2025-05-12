open Nes

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
  | Full of 'a Slug.t * meta * 'a
  | Dummy of 'a
[@@deriving show {with_path = false}]

let is_dummy = function Dummy _ -> true | Full _ -> false

let make' ~slug ?meta value =
  let meta = Option.value ~default: (make_meta ()) meta in
  Full (slug, meta, value)

let make ~slug ?status ?privacy ?created_at ?modified_at value =
  make' ~slug ~meta: (make_meta ?status ?privacy ?created_at ?modified_at ()) value

let make_dummy value = Dummy value

exception UsedGetterOnDummy

let slug = function Full (slug, _, _) -> slug | _ -> raise UsedGetterOnDummy
let slug' e = Slug.unsafe_coerce (slug e)
let slug_as_string e = Slug.to_string (slug e)
let meta = function Full (_, meta, _) -> meta | _ -> raise UsedGetterOnDummy
let value = function Full (_, _, value) | Dummy value -> value

let status' e = status @@ meta e
let privacy' e = privacy @@ meta e

let equal' e f = Slug.equal' (slug e) (slug f)
let equal _ = equal'
let compare' e f = Slug.compare' (slug e) (slug f)
let compare _ = compare'

let yojson_of_t yojson_of_value = function
  | Full (slug, meta, value) ->
    Json.add_field "slug" (Slug.yojson_of_t yojson_of_value slug) @@
      Json.merge_assoc (yojson_of_value value) (yojson_of_meta meta)
  | Dummy v ->
    yojson_of_value v

let yojson_of_t' yojson_of_value = function
  | Full (_, meta, value) -> Json.merge_assoc (yojson_of_value value) (yojson_of_meta meta)
  | Dummy _ -> raise UsedGetterOnDummy

let t_of_yojson' slug value_of_yojson json =
  let (status, json) = Option.value (Json.extract_field_opt "status" json) ~default: (Status.(yojson_of_t bot), json) in
  let (privacy, json) = Option.value (Json.extract_field_opt "privacy" json) ~default: (Privacy.(yojson_of_t default), json) in
  let (created_at, json) = Json.extract_field "created-at" json in
  let (modified_at, value_json) = Json.extract_field "modified-at" json in
  let meta_json = `Assoc [("status", status); ("privacy", privacy); ("created-at", created_at); ("modified-at", modified_at)] in
  let meta = meta_of_yojson meta_json in
  let value = value_of_yojson value_json in
  Full (slug, meta, value)

let t_of_yojson value_of_yojson json =
  match Json.extract_field_opt "slug" json with
  | Some (slug, json) ->
    let slug = Slug.t_of_yojson value_of_yojson slug in
    t_of_yojson' slug value_of_yojson json
  | None ->
    let value = value_of_yojson json in
    Dummy value

module J (A : Madge.JSONABLE) : Madge.JSONABLE with type t = A.t t = struct
  type nonrec t = A.t t
  let yojson_of_t = yojson_of_t A.yojson_of_t
  let t_of_yojson = t_of_yojson A.t_of_yojson
end
