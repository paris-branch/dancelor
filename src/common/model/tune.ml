open Nes
module Madge = Madge_common

type t =
  { slug : t Slug.t ;
    group : TuneGroup.t Slug.t        [@key "tune-group"];
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t Slug.t option [@default None] ;
    sources : string list             [@default []] ; (* FIXME: not string *)
    dances : Dance.t Slug.t list      [@default []] ;
    remark : string                   [@default ""] ;
    disambiguation : string           [@default ""] }
[@@deriving yojson]

let slug t = Lwt.return t.slug
let group t = Lwt.return t.group
let bars t = Lwt.return t.bars
let key t = Lwt.return t.key
let structure t = Lwt.return t.structure
let arranger t = Lwt.return t.arranger
let sources t = Lwt.return t.sources
let dances t = Lwt.return t.dances
let remark t = Lwt.return t.remark
let disambiguation t = Lwt.return t.disambiguation

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val group : t -> TuneGroup.t Lwt.t
  val bars : t -> int Lwt.t
  val key : t -> Music.key Lwt.t
  val structure : t -> string Lwt.t
  val arranger : t -> Credit.t option Lwt.t
  val sources : t -> string list Lwt.t
  val dances : t -> Dance.t list Lwt.t
  val remark : t -> string Lwt.t
  val disambiguation : t -> string Lwt.t

  val content : t -> string Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val all :
    ?filter:TuneFilter.t -> ?pagination:Pagination.t ->
    unit -> t list Lwt.t

  val search :
    ?filter:TuneFilter.t -> ?pagination:Pagination.t ->
    ?threshold:float -> string list ->
    t Score.t list Lwt.t
end

module Arg = struct
  let slug =
    Madge.arg
      ~key:"slug"
      ~serialiser:(Slug.to_yojson ())
      ~unserialiser:(Slug.of_yojson ())

  let filter =
    Madge.arg
      ~key:"filter"
      ~serialiser:TuneFilter.to_yojson
      ~unserialiser:TuneFilter.of_yojson

  let pagination =
    Madge.arg
      ~key:"pagination"
      ~serialiser:Pagination.to_yojson
      ~unserialiser:Pagination.of_yojson

  let threshold =
    Madge.arg
      ~key:"threshold"
      ~serialiser:Madge.float_to_yojson
      ~unserialiser:Madge.float_of_yojson

  let terms =
    Madge.arg
      ~key:"terms"
      ~serialiser:Madge.(list_to_yojson string_to_yojson)
      ~unserialiser:Madge.(list_of_yojson string_of_yojson )
end

module Endpoint = struct
  let get =
    Madge.endpoint
      ~meth:`GET
      ~path:"/tune"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson

  let all =
    Madge.endpoint
      ~meth:`GET
      ~path:"/tune/all"
      ~serialiser:(Madge.list_to_yojson to_yojson)
      ~unserialiser:(Madge.list_of_yojson of_yojson)

  let search =
    Madge.endpoint
      ~meth:`GET
      ~path:"/tune/search"
      ~serialiser:(Madge.list_to_yojson (Score.to_yojson to_yojson))
      ~unserialiser:(Madge.list_of_yojson (Score.of_yojson of_yojson))
end
