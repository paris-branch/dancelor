open Nes

module Self = struct
  type page =
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
      contents    : page list ;
      remark      : string    [@default ""] }
  [@@deriving yojson]

  let _key = "book"
end
include Self

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

module type S = sig
  type page =
    | Version       of Version.t * VersionParameters.t
    | Set           of     Set.t * SetParameters.t
    | InlineSet     of     Set.t * SetParameters.t

  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val title : t -> string Lwt.t
  val subtitle : t -> string Lwt.t
  val date : t -> Date.t Lwt.t
  val contents : t -> page list Lwt.t
  val remark : t -> string Lwt.t

  val contains_set : Set.t Slug.t -> t -> bool
  val compare : t -> t -> int

  (* {2 Warnings} *)

  type warning =
    | Empty
    | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
    | DuplicateVersion of Tune.t

  type warnings = warning list

  val warnings : t -> warnings Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val get_all : unit -> t list Lwt.t

  val search :
    ?pagination:Pagination.t ->
    ?threshold:float ->
    string ->
    t Score.t list Lwt.t
end

module Arg = struct
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/book" (module Self)
  let get_all = endpoint ~path:"/book/all" (module MList (Self))
  let search = endpoint ~path:"/book/search" (module MList (Score.Make_Serialisable (Self)))
end
