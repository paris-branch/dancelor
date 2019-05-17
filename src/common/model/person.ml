open Nes

type t =
  { slug : t Slug.t ;
    name : string }
[@@deriving yojson]

let slug p = Lwt.return p.slug
let name p = Lwt.return p.name
