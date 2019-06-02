open Nes
module Madge = Madge_common

type t =
  { slug : t Slug.t ;
    name : string }
[@@deriving make, yojson]

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
  let slug =
    Madge.arg
      ~key:"slug"
      ~serialiser:(Slug.to_yojson ())
      ~unserialiser:(Slug.of_yojson ())

  let name =
    Madge.arg
      ~key:"name"
      ~serialiser:Madge.string_to_yojson
      ~unserialiser:Madge.string_of_yojson
end

module Endpoint = struct
  let get =
    Madge.endpoint
      ~meth:`GET
      ~path:"/person"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson

  let make_and_save =
    Madge.endpoint
      ~meth:`GET
      ~path:"/person/save"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson
end
