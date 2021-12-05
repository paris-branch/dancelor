open Nes

let _key = "dance"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    kind : Kind.dance ;
    deviser : CreditCore.t Slug.t option [@default None] ;
    two_chords : bool [@default false] [@key "two-chords"] }
[@@deriving yojson]

let slug d = Lwt.return d.slug
let status d = Lwt.return d.status
let name d = Lwt.return d.name
let kind d = Lwt.return d.kind
let deviser d = Lwt.return d.deviser
let two_chords d = Lwt.return d.two_chords

let equal dance1 dance2 =
  let%lwt slug1 = slug dance1 in
  let%lwt slug2 = slug dance2 in
  Lwt.return (Slug.equal slug1 slug2)
