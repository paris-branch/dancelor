open Nes

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    line : string ;
    persons : Person.t Slug.t list [@default []] }
[@@deriving yojson, make]

let _key = "credit"

let slug c = Lwt.return c.slug
let status c = Lwt.return c.status
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons

let is_trad c = c.slug = "traditional"

module Filter = struct
  type t =
    | ExistsPerson of Person.Filter.t
    | ForallPersons of Person.Filter.t
  [@@deriving yojson]

  let _key = "credit-filter"
end
