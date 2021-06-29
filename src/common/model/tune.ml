open Nes

let _key = "tune"

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

let slug tune = Lwt.return tune.slug
let status tune = Lwt.return tune.status
let name tune = Lwt.return tune.name
let alternative_names tune = Lwt.return tune.alternative_names
let kind tune = Lwt.return tune.kind
let author tune = Lwt.return tune.author
let dances tune = Lwt.return tune.dances
let remark tune = Lwt.return tune.remark

let equal tune1 tune2 =
  let%lwt slug1 = slug tune1 in
  let%lwt slug2 = slug tune2 in
  Lwt.return (Slug.equal slug1 slug2)

module Filter = struct
  let _key = "tune-filter"

  type predicate =
    | Is of t
    | Author of Credit.Filter.t (** author is defined and passes the filter *)
    | Kind of Kind.base
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is tune = Formula.pred (Is tune)
  let author cfilter = Formula.pred (Author cfilter)
  let authorIs author_ = author (Credit.Filter.is author_)
  let kind kind = Formula.pred (Kind kind)
end
