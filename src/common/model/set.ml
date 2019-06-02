open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    deviser : Credit.t Slug.t option [@default None] ;
    kind : Kind.dance ;
    status : Status.t                [@default Status.WorkInProgress] ;
    tunes : Tune.t Slug.t list       [@default []] }
[@@deriving make, yojson]

let unsafe_make ~slug ~name ?deviser ~kind ?status ?tunes () =
  let%lwt deviser =
    match deviser with
    | None -> Lwt.return_none
    | Some deviser ->
      let%lwt deviser = Credit.slug deviser in
      Lwt.return_some deviser
  in
  let%lwt tunes =
    match tunes with
    | None -> Lwt.return_none
    | Some tunes ->
      let%lwt tunes = Lwt_list.map_s Tune.slug tunes in
      Lwt.return_some tunes
  in
  Lwt.return (make ~slug ~name ~deviser ~kind ?status ?tunes ())

let slug s = Lwt.return s.slug
let name s = Lwt.return s.name
let deviser s = Lwt.return s.deviser
let kind s = Lwt.return s.kind
let status s = Lwt.return s.status
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
  val name : t -> string Lwt.t
  val deviser : t -> Credit.t option Lwt.t
  val kind : t -> Kind.dance Lwt.t
  val status : t -> Status.t Lwt.t
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
    name:string ->
    ?deviser:Credit.t ->
    kind:Kind.dance ->
    ?status:Status.t ->
    ?tunes:Tune.t list ->
    unit -> t Lwt.t

  val delete : t -> unit Lwt.t
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

  let deviser =
    Madge.arg
      ~key:"deviser"
      ~serialiser:Credit.to_yojson
      ~unserialiser:Credit.of_yojson

  let kind =
    Madge.arg
      ~key:"kind"
      ~serialiser:Kind.dance_to_yojson
      ~unserialiser:Kind.dance_of_yojson

  let status =
    Madge.arg
      ~key:"status"
      ~serialiser:Status.to_yojson
      ~unserialiser:Status.of_yojson

  let tunes =
    Madge.arg
      ~key:"tunes"
      ~serialiser:(Madge.list_to_yojson Tune.to_yojson)
      ~unserialiser:(Madge.list_of_yojson Tune.of_yojson)
end

module Endpoint = struct
  let get =
    Madge.endpoint
      ~meth:`GET
      ~path:"/set"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson

  let get_all =
    Madge.endpoint
      ~meth:`GET
      ~path:"/set/all"
      ~serialiser:(Madge.list_to_yojson to_yojson)
      ~unserialiser:(Madge.list_of_yojson of_yojson)

  let make_and_save =
    Madge.endpoint
      ~meth:`GET
      ~path:"/set/save"
      ~serialiser:to_yojson
      ~unserialiser:of_yojson

  let delete =
    Madge.endpoint
      ~meth:`GET
      ~path:"/set/delete"
      ~serialiser:Madge.unit_to_yojson
      ~unserialiser:Madge.unit_of_yojson
end
