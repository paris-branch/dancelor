open Nes

type t =
  { slug : t Slug.t ;
    line : string ;
    persons : Person.t Slug.t list [@default []] }
[@@deriving yojson]

let slug c = Lwt.return c.slug
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val line : t -> string Lwt.t
  val persons : t -> Person.t list Lwt.t

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
      ~path:"/credit"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson
end
