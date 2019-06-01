open Nes

type t

val slug : t -> t Slug.t Lwt.t
val name : t -> string Lwt.t
val date : t -> Date.t Lwt.t
val status : t -> Status.t Lwt.t
val sets : t -> Set.t Slug.t list Lwt.t

val contains : Set.t Slug.t -> t -> bool
val compare : t -> t -> int

(* {2 Warnings} *)

type warning =
  | Empty
  | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
  | DuplicateTune of TuneGroup.t

type warnings = warning list

(* {2 Unsafe} *)

val to_yojson : t -> NesJson.t
val of_yojson : NesJson.t -> (t, string) result

val warnings_to_yojson : warnings -> NesJson.t
val warnings_of_yojson : NesJson.t -> (warnings, string) result
