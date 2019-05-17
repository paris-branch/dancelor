open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    deviser : Credit.t Slug.t option ;
    kind : Kind.dance ;
    status : Status.t ;
    tunes : Tune.t Slug.t list }
[@@deriving yojson]

let slug s = Lwt.return s.slug
let name s = Lwt.return s.name
let deviser s = Lwt.return s.deviser
let kind s = Lwt.return s.kind
let status s = Lwt.return s.status
let tunes s = Lwt.return s.tunes

let contains t s = List.mem t s.tunes
