open Nes

type meta = {
  status: Status.t; [@default Status.bot]
  created_at: Datetime.t; [@key "created-at"]
  modified_at: Datetime.t; [@key "modified-at"]
}
[@@deriving yojson, show {with_path = false}]

let make_meta ?(status = Status.bot) ?created_at ?modified_at () =
  let now = Datetime.now () in
  {
    status;
    created_at = Option.value ~default: now created_at;
    modified_at = Option.value ~default: now modified_at;
  }

type 'a t =
  | Full of 'a Slug.t * meta * 'a
  | Dummy of 'a
[@@deriving show {with_path = false}]

let is_dummy = function Dummy _ -> true | Full _ -> false

let make' ~slug ?meta value =
  let meta = Option.value ~default: (make_meta ()) meta in
  Full (slug, meta, value)

let make ~slug ?status ?created_at ?modified_at value =
  make' ~slug ~meta: (make_meta ?status ?created_at ?modified_at ()) value

let make_dummy value = Dummy value

exception UsedGetterOnDummy

let on_full e f =
  match e with
  | Full (slug, meta, _value) -> f slug meta
  | Dummy _ -> raise UsedGetterOnDummy

let slug e = on_full e @@ fun slug _ -> Slug.unsafe_coerce slug
let slug' = slug
let status e = on_full e @@ fun _ {status; _} -> status
let created_at e = on_full e @@ fun _ {created_at; _} -> created_at
let modified_at e = on_full e @@ fun _ {modified_at; _} -> modified_at
let value = function Full (_, _, value) | Dummy value -> value

let equal' e f = Slug.equal' (slug e) (slug f)
let equal _ = equal'

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
  let (created_at, json) = Json.extract_field "created-at" json in
  let (modified_at, value_json) = Json.extract_field "modified-at" json in
  let meta_json = `Assoc [("status", status); ("created-at", created_at); ("modified-at", modified_at)] in
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
