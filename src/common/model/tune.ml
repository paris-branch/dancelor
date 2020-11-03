open Nes

module Self = struct
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
end
include Self

let slug tune = Lwt.return tune.slug
let status tune = Lwt.return tune.status
let name tune = Lwt.return tune.name
let alternative_names tune = Lwt.return tune.alternative_names
let kind tune = Lwt.return tune.kind
let author tune = Lwt.return tune.author
let dances tune = Lwt.return tune.dances
let remark tune = Lwt.return tune.remark

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val name : t -> string Lwt.t
  val alternative_names : t -> string list Lwt.t
  val kind : t -> Kind.base Lwt.t
  val author : t -> Credit.t option Lwt.t
  val dances : t -> Dance.t list Lwt.t
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
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/tune" (module Self)
  let search = endpoint ~path:"/tune/search" (module MList (Score.Make_Serialisable (Self)))
end
