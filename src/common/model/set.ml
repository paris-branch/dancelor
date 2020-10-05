open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t                [@default Status.bot] ;
      name : string ;
      deviser : Credit.t Slug.t option [@default None] ;
      kind : Kind.dance ;
      versions : Version.t Slug.t list [@default []] ;
      instructions : string  [@default ""] }
  [@@deriving make, yojson]

  let _key = "set"
end
include Self

let make ?status ~slug ~name ?deviser ~kind ?versions () =
  let%lwt deviser =
    match deviser with
    | None -> Lwt.return_none
    | Some deviser ->
      let%lwt deviser = Credit.slug deviser in
      Lwt.return_some deviser
  in
  let%lwt versions =
    match versions with
    | None -> Lwt.return_none
    | Some versions ->
      let%lwt versions = Lwt_list.map_s Version.slug versions in
      Lwt.return_some versions
  in
  Lwt.return (make ?status ~slug ~name ~deviser ~kind ?versions ())

let make_temp ~name ?deviser ~kind ?versions () =
  make ~slug:Slug.none ~name ?deviser ~kind ?versions ()

let slug s = Lwt.return s.slug
let status s = Lwt.return s.status
let name s = Lwt.return s.name
let deviser s = Lwt.return s.deviser
let kind s = Lwt.return s.kind
let versions s = Lwt.return s.versions
let instructions s = Lwt.return s.instructions

let contains t s = List.mem t s.versions

type warning =
  | Empty
  | WrongKind
  | WrongVersionBars of Version.t
  | WrongVersionKind of Tune.t
  | DuplicateVersion of Tune.t
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
  val versions : t -> Version.t list Lwt.t
  val instructions : t -> string Lwt.t

  val contains : Version.t Slug.t -> t -> bool

  (* {2 Warnings} *)

  type warning =
    | Empty
    | WrongKind
    | WrongVersionBars of Version.t
    | WrongVersionKind of Tune.t
    | DuplicateVersion of Tune.t

  type warnings = warning list

  val warnings : t -> warnings Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Lwt.t

  val all : ?pagination:Pagination.t -> unit -> t list Lwt.t

  val make_temp :
    name:string ->
    ?deviser:Credit.t ->
    kind:Kind.dance ->
    ?versions:Version.t list ->
    unit -> t Lwt.t

  val make_and_save :
    ?status:Status.t ->
    name:string ->
    ?deviser:Credit.t ->
    kind:Kind.dance ->
    ?versions:Version.t list ->
    unit -> t Lwt.t

  val delete : t -> unit Lwt.t

  val search :
    ?pagination:Pagination.t ->
    ?threshold:float ->
    string ->
    t Score.t list Lwt.t

  val count: unit -> int Lwt.t
  (** Number of sets in the database. *)
end

module Arg = struct
  open Madge_common
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let deviser = optarg ~key:"deviser" (module Credit)
  let kind = arg ~key:"kind" (module Kind.Dance)
  let versions = optarg ~key:"versions" (module MList (Version))
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
