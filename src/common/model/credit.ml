open Nes

let _key = "credit"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    line : string ;
    persons : Person.t Slug.t list [@default []] }
[@@deriving yojson, make]

let slug c = Lwt.return c.slug
let status c = Lwt.return c.status
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons

let is_trad c = c.slug = "traditional"

let equal credit1 credit2 =
  let%lwt slug1 = slug credit1 in
  let%lwt slug2 = slug credit2 in
  Lwt.return (Slug.equal slug1 slug2)

module Filter = struct
  let _key = "credit-filter"

  type predicate =
    | Is of t
    | ExistsPerson of Person.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is credit = Formula.pred (Is credit)
  let existsPerson pfilter = Formula.pred (ExistsPerson pfilter)
  let memPerson person = existsPerson (Person.Filter.is person)
  let forallPersons pfilter = Formula.(not_ (existsPerson (not_ pfilter)))
end
