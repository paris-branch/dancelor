open Nes

let _key = "book"

type page_slug =
  | Version       of VersionCore.t Slug.t * VersionParameters.t
  | Set           of     SetCore.t Slug.t * SetParameters.t
  | InlineSet     of     SetCore.t        * SetParameters.t
[@@deriving yojson]

type t =
  { slug        : t Slug.t ;
    status      : Status.t   [@default Status.bot] ;
    title       : string ;
    subtitle    : string     [@default ""] ;
    short_title : string     [@default ""] [@key "short-title"] ;
    date        : Date.t     [@default Date.none] ;
    contents    : page_slug list ;
    source      : bool       [@default false] ;
    remark      : string     [@default ""] ;
    scddb_id    : int option [@default None] [@key "scddb-id"] }
[@@deriving yojson]

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let title p = Lwt.return p.title
let short_title p = if p.short_title = "" then title p else Lwt.return p.short_title
let subtitle p = Lwt.return p.subtitle
let date p = Lwt.return p.date
let contents p = Lwt.return p.contents
let source p = Lwt.return p.source
let remark p = Lwt.return p.remark

let equal book1 book2 =
  let%lwt slug1 = slug book1 in
  let%lwt slug2 = slug book2 in
  Lwt.return (Slug.equal slug1 slug2)

let is_source p = source p

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
  | DuplicateSet of SetCore.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of TuneCore.t * (SetCore.t option * int) list
  (* DuplicateVersion contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

type page =
  | Version   of VersionCore.t * VersionParameters.t
  | Set       of     SetCore.t * SetParameters.t
  | InlineSet of     SetCore.t * SetParameters.t
