open Nes

type page_slug =
  | Version       of Version.t Slug.t * VersionParameters.t
  | Set           of     Set.t Slug.t * SetParameters.t
  | InlineSet     of     Set.t        * SetParameters.t
[@@deriving yojson]

type t =
  { slug        : t Slug.t ;
    status      : Status.t  [@default Status.bot] ;
    title       : string ;
    subtitle    : string    [@default ""] ;
    short_title : string    [@default ""] [@key "short-title"] ;
    date        : Date.t    [@default Date.none] ;
    contents    : page_slug list ;
    remark      : string    [@default ""] }
[@@deriving yojson]

let _key = "book"

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let title p = Lwt.return p.title
let subtitle p = Lwt.return p.subtitle
let date p = Lwt.return p.date
let contents p = Lwt.return p.contents
let remark p = Lwt.return p.remark

let contains_set slug1 p =
  List.exists
    (function
      | Set (slug2, _) -> Slug.equal slug1 slug2
      | _ -> false)
    p.contents

let compare p1 p2 =
  (* Compare first by date *)
  let c = compare p1.date p2.date in
  if c = 0 then
    compare p1 p2
  else
    c

type warning =
  | Empty
  | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of Tune.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

type page =
  | Version       of Version.t * VersionParameters.t
  | Set           of     Set.t * SetParameters.t
  | InlineSet     of     Set.t * SetParameters.t
