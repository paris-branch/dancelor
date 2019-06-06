open Nes

module Self = struct
  type t =
    { slug : t NesSlug.t ;
      name : string }
  [@@deriving make, yojson]

  let _key = "source"
end
include Self

let slug s = Lwt.return s.slug
let name s = Lwt.return s.name

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val name : t -> string Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val make_and_save :
    name:string ->
    unit -> t Lwt.t

  val search :
    ?threshold:float ->
    string list ->
    t Score.t list Lwt.t
end

module Arg = struct
  let slug = Madge_common.(arg ~key:"slug" (module MString))
  let name = Madge_common.(arg ~key:"name" (module MString))
  let threshold = Madge_common.(optarg ~key:"threshold" (module MFloat))
  let terms = Madge_common.(arg ~key:"terms" (module MList(MString)))
end

module Endpoint = struct
  let get = Madge_common.endpoint ~path:"/source" (module Self)
  let make_and_save = Madge_common.endpoint ~path:"/source/save" (module Self)
  let search = Madge_common.(endpoint ~path:"/source/search" (module MList (Score.Make_Serialisable (Self))))
end
