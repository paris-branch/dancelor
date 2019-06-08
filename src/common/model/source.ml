open Nes

module Self = struct
  type t =
    { slug : t NesSlug.t ;
      status : Status.t [@default Status.bot] ;
      name : string }
  [@@deriving make, yojson]

  let _key = "source"
end
include Self

let slug s = Lwt.return s.slug
let status s = Lwt.return s.status
let name s = Lwt.return s.name

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val name : t -> string Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val make_and_save :
    ?status:Status.t ->
    name:string ->
    unit -> t Lwt.t

  val search :
    ?pagination:Pagination.t ->
    ?threshold:float ->
    string ->
    t Score.t list Lwt.t
end

module Arg = struct
  let slug = Madge_common.(arg ~key:"slug" (module MString))
  let status = Madge_common.optarg (module Status)
  let name = Madge_common.(arg ~key:"name" (module MString))
  let pagination = Madge_common.(optarg (module Pagination))
  let threshold = Madge_common.(optarg ~key:"threshold" (module MFloat))
  let string = Madge_common.(arg (module MString))
end

module Endpoint = struct
  let get = Madge_common.endpoint ~path:"/source" (module Self)
  let make_and_save = Madge_common.endpoint ~path:"/source/save" (module Self)
  let search = Madge_common.(endpoint ~path:"/source/search" (module MList (Score.Make_Serialisable (Self))))
end
