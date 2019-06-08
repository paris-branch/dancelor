open Nes
module Madge = Madge_common

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t                [@default Status.bot] ;
      name : string ;
      deviser : Credit.t Slug.t option [@default None] ;
      kind : Kind.dance ;
      tunes : Tune.t Slug.t list       [@default []] }
  [@@deriving make, yojson]

  let _key = "set"
end
include Self

let make ?status ~slug ~name ?deviser ~kind ?tunes () =
  Lwt.return (make ?status ~slug ~name ~deviser ~kind ?tunes ())

let slug s = Lwt.return s.slug
let status s = Lwt.return s.status
let name s = Lwt.return s.name
let deviser s = Lwt.return s.deviser
let kind s = Lwt.return s.kind
let tunes s = Lwt.return s.tunes

let contains t s = List.mem t s.tunes

type warning =
  | Empty
  | WrongKind
  | WrongTuneBars of Tune.t
  | WrongTuneKind of TuneGroup.t
  | DuplicateTune of TuneGroup.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

module type S = sig
  type nonrec t = t

  val slug : t -> t Slug.t Lwt.t
  val status : t -> Status.t Lwt.t
  val name : t -> string Lwt.t
  val deviser : t -> Credit.t option Lwt.t
  val kind : t -> Kind.dance Lwt.t
  val tunes : t -> Tune.t list Lwt.t

  val contains : Tune.t Slug.t -> t -> bool

  (* {2 Warnings} *)

  type warning =
    | Empty
    | WrongKind
    | WrongTuneBars of Tune.t
    | WrongTuneKind of TuneGroup.t
    | DuplicateTune of TuneGroup.t

  type warnings = warning list

  val warnings : t -> warnings Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val get_all : unit -> t list Lwt.t

  val make_and_save :
    ?status:Status.t ->
    name:string ->
    ?deviser:Credit.t ->
    kind:Kind.dance ->
    ?tunes:Tune.t list ->
    unit -> t Lwt.t

  val delete : t -> unit Lwt.t

  val search :
    ?pagination:Pagination.t ->
    ?threshold:float ->
    string ->
    t Score.t list Lwt.t
end

module Arg = struct
  let slug = Madge_common.(arg ~key:"slug" (module MString))
  let status = Madge_common.optarg (module Status)
  let name = Madge_common.(arg ~key:"name" (module MString))
  let deviser = Madge_common.optarg ~key:"deviser" (module Credit)
  let kind = Madge_common.arg ~key:"kind" (module Kind.Dance)
  let tunes = Madge_common.(optarg ~key:"tunes" (module MList (Tune)))
  let pagination = Madge_common.optarg (module Pagination)
  let threshold = Madge_common.(optarg ~key:"threshold" (module MFloat))
  let string = Madge_common.(arg (module MString))
end

module Endpoint = struct
  let get = Madge_common.endpoint ~path:"/set" (module Self)
  let get_all = Madge_common.(endpoint ~path:"/set/all" (module MList (Self)))
  let make_and_save = Madge_common.endpoint ~path:"/set/save" (module (Self))
  let delete = Madge.(endpoint ~path:"/set/delete" (module MUnit))
  let search = Madge_common.(endpoint ~path:"/set/search" (module MList (Score.Make_Serialisable (Self))))
end
