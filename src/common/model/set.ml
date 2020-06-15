open Nes

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

  val all : ?pagination:Pagination.t -> unit -> t list Lwt.t

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

  val count:
    unit -> int Lwt.t
end

module Arg = struct
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let deviser = optarg ~key:"deviser" (module Credit)
  let kind = arg ~key:"kind" (module Kind.Dance)
  let tunes = optarg ~key:"tunes" (module MList (Tune))
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

module Endpoint = struct
  open Madge_common
  let get = endpoint ~path:"/set" (module Self)
  let all = endpoint ~path:"/set/all" (module MList (Self))
  let make_and_save = endpoint ~path:"/set/save" (module (Self))
  let delete = endpoint ~path:"/set/delete" (module MUnit)
  let search = endpoint ~path:"/set/search" (module MList (Score.Make_Serialisable (Self)))
  let count = endpoint ~path:"/set/count" (module MInteger)
end
