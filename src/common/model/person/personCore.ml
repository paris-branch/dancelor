open Nes

let _key = "person"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving yojson, make]

module Filter = struct
  let _key = "person-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
