open Nes

type t =
  { slug : t Slug.t ;
    line : string ;
    persons : Person.t Slug.t list [@default []] }
[@@deriving make, yojson]

let slug c = Lwt.return c.slug
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons

let unsafe_make ~slug ~line ?persons () =
  Lwt.return (make ~slug ~line ?persons ())
