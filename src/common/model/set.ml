open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    deviser : Credit.t Slug.t option ;
    kind : Kind.dance ;
    status : Status.t ;
    tunes : Tune.t Slug.t list }
[@@deriving yojson]

let slug s = s.slug
let name s = s.name
let kind s = s.kind
let tunes s = s.tunes
let deviser s = s.deviser
let contains t s = List.mem t s.tunes
