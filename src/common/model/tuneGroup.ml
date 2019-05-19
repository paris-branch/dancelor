open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    kind : Kind.base ;
    author : Credit.t Slug.t option [@default None] ;
    remark : string                 [@default ""] }
[@@deriving yojson]

let slug g = Lwt.return g.slug
let name g = Lwt.return g.name
let kind g = Lwt.return g.kind
let author g = Lwt.return g.author
let remark g = Lwt.return g.remark
