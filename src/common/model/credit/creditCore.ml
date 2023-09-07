open Nes

let _key = "credit"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    line : string ;
    persons : PersonCore.t Slug.t list [@default []];
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving yojson, make]

let slug c = Lwt.return c.slug
let status c = Lwt.return c.status
let persons c = Lwt.return c.persons

module Filter = struct
  let _key = "credit-filter"

  type predicate =
    | Is of t
    | Line of string
    | LineMatches of string
    | ExistsPerson of PersonCore.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
