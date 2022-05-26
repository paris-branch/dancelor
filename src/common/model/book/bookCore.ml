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
[@@deriving make, yojson]

let slug book = Lwt.return book.slug
let status book = Lwt.return book.status
let title book = Lwt.return book.title
let subtitle book = Lwt.return book.subtitle
let short_title book = if book.short_title = "" then title book else Lwt.return book.short_title
let date book = Lwt.return book.date
let contents book = Lwt.return book.contents
let source book = Lwt.return book.source (* FIXME: Should be removed *)
let remark book = Lwt.return book.remark
let scddb_id book = Lwt.return book.scddb_id

let equal book1 book2 =
  let%lwt slug1 = slug book1 in
  let%lwt slug2 = slug book2 in
  Lwt.return (Slug.equal slug1 slug2)

let is_source book = source book

let contains_set set1 book =
  List.exists
    (function
      | Set (set2, _) -> Slug.equal set1 set2
      | _ -> false)
    book.contents

let compare book1 book2 =
  (* Compare first by date *)
  let c = compare book1.date book2.date in
  if c = 0 then
    compare book1 book2
  else
    c

type warning =
  | Empty
  | DuplicateSet of SetCore.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of TuneCore.t * (SetCore.t option * int) list
  (* DuplicateVersion contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
  | SetDanceMismatch of SetCore.t * DanceCore.t
  (* SetDanceMismatch contains a set where one of the associated dances
     does not have the same kind *)
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

type page =
  | Version   of VersionCore.t * VersionParameters.t
  | Set       of     SetCore.t * SetParameters.t
  | InlineSet of     SetCore.t * SetParameters.t
[@@deriving yojson]

let make ?status ~slug ~title ?date ?contents_and_parameters () =
  let%lwt contents_and_parameters =
    let%olwt contents_and_parameters = Lwt.return contents_and_parameters in
    let%lwt contents_and_parameters =
      Lwt_list.map_s
        (fun page ->
          match page with
          | Version (version, params) ->
              let%lwt slug = VersionCore.slug version in
              Lwt.return (Version (slug, params) : page_slug)
          | Set (set, params) ->
              let%lwt slug = SetCore.slug set in
              Lwt.return (Set (slug, params) : page_slug)
          | InlineSet (set, params) ->
              Lwt.return (InlineSet (set, params) : page_slug)
        )
        contents_and_parameters
    in Lwt.return_some contents_and_parameters
  in
  Lwt.return (make ?status ~slug ~title ?date ?contents:contents_and_parameters ())
