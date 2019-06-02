open Nes
module Madge = Madge_common

type t =
  { slug : t Slug.t ;
    name : string ;
    kind : Kind.dance ;
    deviser : Credit.t Slug.t option [@default None] }
[@@deriving yojson]

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
  let slug =
    Madge.arg
      ~key:"slug"
      ~serialiser:(Slug.to_yojson ())
      ~unserialiser:(Slug.of_yojson ())
end

module Endpoint = struct
  let get =
    Madge.endpoint
      ~meth:`GET
      ~path:"/dance"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson
end
