open Nes

let _key = "person"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    modified_at : NesDate.t [@key "modified-at"] }
[@@deriving yojson, make]

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let name p = Lwt.return p.name

let equal person1 person2 =
  let%lwt slug1 = slug person1 in
  let%lwt slug2 = slug person2 in
  Lwt.return (Slug.equal slug1 slug2)
