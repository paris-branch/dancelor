open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      line : string ;
      persons : Person.t Slug.t list [@default []] }
  [@@deriving yojson, make]

  let _key = "credit"
end
include Self

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

module Arg = struct
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let line = arg ~key:"line" (module MString)
  let persons = optarg ~key:"persons" (module MList(Person))
  let pagination = optarg (module Pagination)
  let filter = optarg (module Filter)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/credit" (module Self)
  let all = endpoint ~path:"/credit/all" (module MList (Self))
  let make_and_save = endpoint ~path:"/credit/save" (module Self)
  let search = endpoint ~path:"/credit/search" (module MList (Score.Make_Serialisable (Self)))
end
