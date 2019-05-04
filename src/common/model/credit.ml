open Nes

type t =
  { slug : t Slug.t ;
    line : string ;
    persons : Person.t Slug.t list }
[@@deriving yojson]

let slug c = c.slug
let line c = c.line
