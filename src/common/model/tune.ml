open Nes

type t =
  { slug : t Slug.t ;
    status : Status.t               [@default Status.bot] ;
    name : string ;
    alternative_names : string list [@key "alternative-names"] [@default []] ;
    kind : Kind.base ;
    author : Credit.t Slug.t option [@default None] ;
    dances : Dance.t Slug.t list    [@default []] ;
    remark : string                 [@default ""] }
[@@deriving yojson]

let _key = "tune"

let slug tune = Lwt.return tune.slug
let status tune = Lwt.return tune.status
let name tune = Lwt.return tune.name
let alternative_names tune = Lwt.return tune.alternative_names
let kind tune = Lwt.return tune.kind
let author tune = Lwt.return tune.author
let dances tune = Lwt.return tune.dances
let remark tune = Lwt.return tune.remark

module Filter = struct
  let _key = "tune-filter"

  type tune = t
  [@@deriving yojson]

  type t =
    | Is of tune
    | Author of Credit.Filter.t
    | AuthorIsDefined
    | Kind of Kind.base
  [@@deriving yojson]
end
