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

let to_yojson value_to_yojson = function
  | Full (slug, meta, value) ->
    Json.add_field "slug" (Slug.to_yojson value_to_yojson slug) @@
      Json.merge_assoc (value_to_yojson value) (meta_to_yojson meta)
  | Dummy v ->
    value_to_yojson v

let to_yojson' value_to_yojson = function
  | Full (_, meta, value) -> Json.merge_assoc (value_to_yojson value) (meta_to_yojson meta)
  | Dummy _ -> raise UsedGetterOnDummy

let of_yojson' slug value_of_yojson json =
  let (status, json) = Option.value (Json.extract_field_opt "status" json) ~default: (Status.(to_yojson bot), json) in
  let (privacy, json) = Option.value (Json.extract_field_opt "privacy" json) ~default: (Privacy.(to_yojson default), json) in
  let (created_at, json) = Json.extract_field "created-at" json in
  let (modified_at, value_json) = Json.extract_field "modified-at" json in
  let meta_json = `Assoc [("status", status); ("privacy", privacy); ("created-at", created_at); ("modified-at", modified_at)] in
  Result.bind (meta_of_yojson meta_json) @@ fun meta ->
  Result.bind (value_of_yojson value_json) @@ fun value ->
  Ok (Full (slug, meta, value))

let of_yojson value_of_yojson json =
  match Json.extract_field_opt "slug" json with
  | Some (slug, json) ->
    Result.bind (Slug.of_yojson value_of_yojson slug) @@ fun slug ->
    of_yojson' slug value_of_yojson json
  | None -> Result.map (fun x -> Dummy x) (value_of_yojson json)

module J (A : Madge.JSONABLE) : Madge.JSONABLE with type t = A.t t = struct
  type nonrec t = A.t t
  let to_yojson = to_yojson A.to_yojson
  let of_yojson = of_yojson A.of_yojson
end
