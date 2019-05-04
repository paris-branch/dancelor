open Nes

type t =
  { slug : t Slug.t ;
    name : string }
[@@deriving yojson]

let slug p = p.slug
let name p = p.name
