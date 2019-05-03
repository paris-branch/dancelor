open Nes
open Protocol_conv_jsonm

type t =
  { slug : t Slug.t ;
    name : string }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "person"))
let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let serialize person =
  `O [
      "slug", `String person.slug ;
      "name", `String person.name
    ]

let unserialize json =
  { slug = Slug.from_string Json.(get ~k:string ["slug"] json) ;
    name = Json.(get ~k:string ["name"] json) }

let slug p = p.slug
let name p = p.name
