open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    kind : Kind.base ;
    author : Credit.t Slug.t option [@default None] ;
    remark : string                 [@default ""] }
[@@deriving yojson]

let slug g = Lwt.return g.slug
let name g = Lwt.return g.name
let kind g = Lwt.return g.kind
let author g = Lwt.return g.author
let remark g = Lwt.return g.remark

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val name : t -> string Lwt.t
  val kind : t -> Kind.base Lwt.t
  val author : t -> Credit.t option Lwt.t
  val remark : t -> string Lwt.t

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
      ~path:"/tune-group"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson
end
