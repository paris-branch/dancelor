open Nes

type t =
  { slug : t Slug.t ;
    line : string ;
    persons : Person.t Slug.t list [@default []] }
[@@deriving yojson]

let slug c = Lwt.return c.slug
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons
