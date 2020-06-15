open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t                 [@default Status.bot] ;
      group : TuneGroup.t Slug.t        [@key "tune-group"];
      bars : int ;
      key : Music.key ;
      structure : string ;
      arranger : Credit.t Slug.t option [@default None] ;
      sources : Source.t Slug.t list    [@default []] ;
      remark : string                   [@default ""] ;
      disambiguation : string           [@default ""] }
  [@@deriving yojson]

  let _key = "tune"
end
include Self

let slug t = Lwt.return t.slug
let status t = Lwt.return t.status
let group t = Lwt.return t.group
let bars t = Lwt.return t.bars
let key t = Lwt.return t.key
let structure t = Lwt.return t.structure
let arranger t = Lwt.return t.arranger
let sources t = Lwt.return t.sources
let remark t = Lwt.return t.remark
let disambiguation t = Lwt.return t.disambiguation

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val group : t -> TuneGroup.t Lwt.t
  val bars : t -> int Lwt.t
  val key : t -> Music.key Lwt.t
  val structure : t -> string Lwt.t
  val arranger : t -> Credit.t option Lwt.t
  val sources : t -> Source.t list Lwt.t
  val remark : t -> string Lwt.t
  val disambiguation : t -> string Lwt.t

  val content : t -> string Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val all :
    ?filter:TuneFilter.t -> ?pagination:Pagination.t ->
    unit -> t list Lwt.t

  val search :
    ?filter:TuneFilter.t ->
    ?pagination:Pagination.t ->
    ?threshold:float -> string ->
    t Score.t list Lwt.t

  val count :
    ?filter:TuneFilter.t -> 
    unit -> int Lwt.t
end

module Arg = struct
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let filter = optarg (module TuneFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/tune" (module Self)
  let all = endpoint ~path:"/tune/all" (module MList (Self))
  let search = endpoint ~path:"/tune/search" (module MList (Score.Make_Serialisable (Self)))
  let count = endpoint ~path:"/tune/count" (module MInteger)
end
