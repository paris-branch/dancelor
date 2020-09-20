open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      line : string ;
      persons : Person.t Slug.t list [@default []] }
  [@@deriving make, yojson]

  let _key = "credit"
end
include Self

let slug c = Lwt.return c.slug
let status c = Lwt.return c.status
let line c = Lwt.return c.line
let persons c = Lwt.return c.persons

let is_trad c = c.slug = "trad"

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val line : t -> string Lwt.t
  val persons : t -> Person.t list Lwt.t

  val is_trad : t -> bool

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val make_and_save :
    ?status:Status.t ->
    line:string ->
    ?persons:Person.t list ->
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
  let line = arg ~key:"line" (module MString)
  let persons = optarg ~key:"persons" (module MList(Person))
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/credit" (module Self)
  let make_and_save = endpoint ~path:"/credit/save" (module Self)
  let search = endpoint ~path:"/credit/search" (module MList (Score.Make_Serialisable (Self)))
end
