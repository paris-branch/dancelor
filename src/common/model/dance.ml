open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      name : string ;
      kind : Kind.dance ;
      deviser : Credit.t Slug.t option [@default None] }
  [@@deriving yojson]

  let _key = "dance"
end
include Self

let slug d = Lwt.return d.slug
let name d = Lwt.return d.name
let kind d = Lwt.return d.kind
let deviser d = Lwt.return d.deviser

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val name : t -> string Lwt.t
  val kind : t -> Kind.dance Lwt.t
  val deviser : t -> Credit.t option Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t
end

module Arg = struct
  let slug = Madge_common.(arg ~key:"slug" (module MString))
end

module Endpoint = struct
  let get = Madge_common.endpoint ~path:"/dance" (module Self)
end
