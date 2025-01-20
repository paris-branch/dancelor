open Nes

(* FIXME: We can get rid of this recursive type by getting rid of the ['a t
   Slug.t] field, which I think we should do for cleanliness anyway. *)

type 'a full = {
  slug: 'a t Slug.t;
  status: Status.t;
  created_at: Datetime.t; [@key "created-at"]
  modified_at: Datetime.t; [@key "modified-at"]
  value: 'a;
}

and 'a t =
  | Full of 'a full
  | Dummy of 'a
[@@deriving yojson, show {with_path = false}]

let is_dummy = function Dummy _ -> true | Full _ -> false

let make ~slug ?(status = Status.bot) ?created_at ?modified_at value =
  let now = Datetime.now () in
  Full
    {
      slug;
      status;
      value;
      created_at = Option.value ~default: now created_at;
      modified_at = Option.value ~default: now modified_at;
    }

let make_dummy value = Dummy value

exception UsedGetterOnDummy

let on_full e f =
  match e with
  | Full e -> f e
  | Dummy _ -> raise UsedGetterOnDummy

let slug e = on_full e @@ fun {slug; _} -> Slug.unsafe_coerce slug
let slug' = slug

let status e = on_full e @@ fun {status; _} -> status
let created_at e = on_full e @@ fun {created_at; _} -> created_at
let modified_at e = on_full e @@ fun {modified_at; _} -> modified_at

let value = function
  | Full {value; _} -> value
  | Dummy value -> value

let equal' e f = Slug.equal' (slug e) (slug f)
let equal _ = equal'

let to_yojson value_to_yojson = function
  | Full e ->
    full_to_yojson value_to_yojson e
    |> Json.extract_field "value"
    |> uncurry Json.merge_assoc
  | Dummy v ->
    value_to_yojson v

let of_yojson value_of_yojson json =
  match Json.extract_field_opt "slug" json with
  | Some (slug, json) ->
    let (status, json) = Option.value (Json.extract_field_opt "status" json) ~default: (Status.(to_yojson bot), json) in
    let (created_at, json) = Json.extract_field "created-at" json in
    let (modified_at, json) = Json.extract_field "modified-at" json in
    Result.map (fun x -> Full x) @@
    full_of_yojson value_of_yojson @@
    `Assoc
      [
        ("slug", slug);
        ("status", status);
        ("created-at", created_at);
        ("modified-at", modified_at);
        ("value", json);
      ]
  | None -> Result.map (fun x -> Dummy x) (value_of_yojson json)
