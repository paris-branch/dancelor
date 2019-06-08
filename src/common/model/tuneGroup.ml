open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      name : string ;
      alt_names : string list [@key "alt-names"] [@default []] ;
      kind : Kind.base ;
      author : Credit.t Slug.t option [@default None] ;
      remark : string                 [@default ""] }
  [@@deriving yojson]

  let _key = "tune-group"
end
include Self

let slug g = Lwt.return g.slug
let status g = Lwt.return g.status
let name g = Lwt.return g.name
let alt_names g = Lwt.return g.alt_names
let kind g = Lwt.return g.kind
let author g = Lwt.return g.author
let remark g = Lwt.return g.remark

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val name : t -> string Lwt.t
  val alt_names : t -> string list Lwt.t
  val kind : t -> Kind.base Lwt.t
  val author : t -> Credit.t option Lwt.t
  val remark : t -> string Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val search :
    ?pagination:Pagination.t ->
    ?threshold:float ->
    string ->
    t Score.t list Lwt.t
end

module Arg = struct
  let slug = Madge_common.(arg ~key:"slug" (module MString))
  let status = Madge_common.optarg (module Status)
  let pagination = Madge_common.(optarg (module Pagination))
  let threshold = Madge_common.(optarg ~key:"threshold" (module MFloat))
  let string = Madge_common.(arg (module MString))
end

module Endpoint = struct
  let get = Madge_common.endpoint ~path:"/tune-group" (module Self)
  let search = Madge_common.(endpoint ~path:"/tune-group/search" (module MList (Score.Make_Serialisable (Self))))
end
