open Nes

type t =
  { slug : t NesSlug.t ;
    status : Status.t [@default Status.bot] ;
    name : string }
[@@deriving make, yojson]

let _key = "source"

let slug s = Lwt.return s.slug
let status s = Lwt.return s.status
let name s = Lwt.return s.name
