type t =
  { slug : t NesSlug.t ;
    name : string ;
    kind : Kind.dance ;
    deviser : Credit.t NesSlug.t option [@default None] }
[@@deriving yojson]

let slug d = Lwt.return d.slug
let name d = Lwt.return d.name
let kind d = Lwt.return d.kind
let deviser d = Lwt.return d.deviser
