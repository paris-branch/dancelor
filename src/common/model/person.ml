open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      name : string }
  [@@deriving make, yojson]

  let _key = "person"
end
include Self

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let name p = Lwt.return p.name

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
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/person" (module Self)
  let make_and_save = endpoint ~path:"/person/save" (module Self)
  let search = endpoint ~path:"/person/search" (module MList (Score.Make_Serialisable (Self)))
end
