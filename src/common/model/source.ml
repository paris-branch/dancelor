open Nes

let _key = "source"

type t =
  { slug : t NesSlug.t ;
    status : Status.t [@default Status.bot] ;
    name : string }
[@@deriving make, yojson]

let slug s = Lwt.return s.slug
let status s = Lwt.return s.status
let name s = Lwt.return s.name

let equal source1 source2 =
  let%lwt slug1 = slug source1 in
  let%lwt slug2 = slug source2 in
  Lwt.return (Slug.equal slug1 slug2)
