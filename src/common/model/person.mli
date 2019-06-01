open Nes

type t

val slug : t -> t Slug.t Lwt.t
val name : t -> string Lwt.t

(* {2 Unsafe} *)

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val unsafe_make : slug:t Slug.t -> name:string -> unit -> t Lwt.t
