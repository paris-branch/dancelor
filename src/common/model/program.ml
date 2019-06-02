open Nes
module Madge = Madge_common

type t =
  { slug : t Slug.t ;
    name : string ;
    date : Date.t ;
    status : Status.t ;
    sets : Set.t Slug.t list }
[@@deriving yojson]

let slug p = Lwt.return p.slug
let name p = Lwt.return p.name
let date p = Lwt.return p.date
let status p = Lwt.return p.status
let sets p = Lwt.return p.sets

let contains s p = List.mem s p.sets

let compare p1 p2 =
  (* Compare first by date *)
  let c = compare p1.date p2.date in
  if c = 0 then
    compare p1 p2
  else
    c

type warning =
  | Empty
  | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
  | DuplicateTune of TuneGroup.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val name : t -> string Lwt.t
  val date : t -> Date.t Lwt.t
  val status : t -> Status.t Lwt.t
  val sets : t -> Set.t list Lwt.t

  val contains : Set.t Slug.t -> t -> bool
  val compare : t -> t -> int

  (* {2 Warnings} *)

  type warning =
    | Empty
    | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
    | DuplicateTune of TuneGroup.t

  type warnings = warning list

  val warnings : t -> warnings Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val get_all : unit -> t list Lwt.t
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
      ~path:"/program"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson

  let get_all =
    Madge.endpoint
      ~meth:`GET
      ~path:"/program/all"
      ~serialiser:(Madge.list_to_yojson to_yojson)
      ~unserialiser:(Madge.list_of_yojson of_yojson)
end
