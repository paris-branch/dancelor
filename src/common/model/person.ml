open Nes

type t =
  { slug : t Slug.t ;
    name : string }
[@@deriving make, yojson]

let slug p = Lwt.return p.slug
let name p = Lwt.return p.name

let unsafe_make ~slug ~name () =
  Lwt.return (make ~slug ~name)
