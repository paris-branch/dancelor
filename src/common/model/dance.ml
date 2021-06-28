open Nes

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    kind : Kind.dance ;
    deviser : Credit.t Slug.t option [@default None] }
[@@deriving yojson]

let _key = "dance"

let slug d = Lwt.return d.slug
let status d = Lwt.return d.status
let name d = Lwt.return d.name
let kind d = Lwt.return d.kind
let deviser d = Lwt.return d.deviser
