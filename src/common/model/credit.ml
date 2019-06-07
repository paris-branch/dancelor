open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      line : string ;
      persons : Person.t Slug.t list [@default []] }
  [@@deriving make, yojson]

  let _key = "credit"
end
include Self

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

  val make_and_save :
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
  let slug = Madge_common.(arg ~key:"slug" (module MString))
  let line = Madge_common.(arg ~key:"line" (module MString))
  let persons = Madge_common.(optarg ~key:"persons" (module MList(Person)))
  let pagination = Madge_common.(optarg (module Pagination))
  let threshold = Madge_common.(optarg ~key:"threshold" (module MFloat))
  let string = Madge_common.(arg (module MString))
end

module Endpoint = struct
  let get = Madge_common.endpoint ~path:"/credit" (module Self)
  let make_and_save = Madge_common.endpoint ~path:"/credit/save" (module Self)
  let search = Madge_common.(endpoint ~path:"/credit/search" (module MList (Score.Make_Serialisable (Self))))
end
