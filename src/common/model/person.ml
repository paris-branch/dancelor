open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      name : string }
  [@@deriving make, yojson]

  let _key = "person"
end
include Self

let slug p = Lwt.return p.slug
let name p = Lwt.return p.name

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val name : t -> string Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val make_and_save :
    name:string ->
    unit -> t Lwt.t
end

module Arg = struct
  let slug = Madge_common.(arg ~key:"slug" (module MString))
  let name = Madge_common.(arg ~key:"name" (module MString))
end

module Endpoint = struct
  let get = Madge_common.endpoint ~path:"/person" (module Self)
  let make_and_save = Madge_common.endpoint ~path:"/person/save" (module Self)
end
