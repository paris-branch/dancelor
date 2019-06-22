open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      name : string ;
      kind : Kind.dance ;
      deviser : Credit.t Slug.t option [@default None] ;
      originals : Tune.t Slug.t list [@default []] }
  [@@deriving yojson]

  let _key = "dance"
end
include Self

let slug d = Lwt.return d.slug
let status d = Lwt.return d.status
let name d = Lwt.return d.name
let kind d = Lwt.return d.kind
let deviser d = Lwt.return d.deviser
let originals d = Lwt.return d.originals

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val name : t -> string Lwt.t
  val kind : t -> Kind.dance Lwt.t
  val deviser : t -> Credit.t option Lwt.t
  val originals : t -> TuneGroup.t list Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

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
  let get = endpoint ~path:"/dance" (module Self)
  let search = endpoint ~path:"/dance/search" (module MList (Score.Make_Serialisable (Self)))
end
