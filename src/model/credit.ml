open Dancelor_common open Option
open Protocol_conv_jsonm

type t =
  { slug : t Slug.t ;
    line : string ;
    persons : Person.t Slug.t list }
[@@deriving protocol ~driver:(module Jsonm)]

let to_jsonm = to_jsonm ||> Json.on_value (Json.add_field "type" (`String "credit"))
let to_json = to_jsonm ||> Json.of_value

let of_json = Json.to_value ||> of_jsonm

let serialize credit =
  `O [
      "slug", `String credit.slug;
      "line", `String credit.line;
      "persons", `A (List.map (fun slug -> `String slug) credit.persons)
    ]

let unserialize json =
  {
    slug = Json.(get ~k:slug ["slug"] json) ;
    line = Json.(get ~k:string ["line"] json) ;
    persons = Json.(get ~k:(strings >=> (List.map Slug.from_string ||> wrap)) ["persons"] json) ;
  }

let slug c = c.slug
let line c = c.line
