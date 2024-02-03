open Nes

let _key = "person"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving yojson, make]

(* FIXME: PPX *)
let slug person = person.slug
let status person = person.status
let name person = person.name
let scddb_id person = person.scddb_id
let modified_at person = person.modified_at
let created_at person = person.created_at

module Filter = struct
  let _key = "person-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
  [@@deriving yojson]

  (* FIXME: PPX *)
  let is person = Is person
  let name name = Name name
  let nameMatches name = NameMatches name

  type t = predicate Formula.t
  [@@deriving yojson]

  let is' = Formula.pred % is
  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
end
