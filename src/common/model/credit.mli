open Nes

type t

val slug : t -> t Slug.t Lwt.t
val line : t -> string Lwt.t
val persons : t -> Person.t Slug.t list Lwt.t

(* {2 Unsafe} *)

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val unsafe_make :
  slug:t Slug.t ->
  line:string ->
  ?persons:Person.t Slug.t list ->
  unit -> t Lwt.t
