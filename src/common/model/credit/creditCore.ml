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
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons
let scddb_id c = Lwt.return c.scddb_id

let trad_slug = Slug.unsafe_of_string "traditional"
let is_trad c = Slug.equal c.slug trad_slug

let equal credit1 credit2 =
  let%lwt slug1 = slug credit1 in
  let%lwt slug2 = slug credit2 in
  Lwt.return (Slug.equal slug1 slug2)
