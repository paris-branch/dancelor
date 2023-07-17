open Nes

(** {1 Dance Kind} *)

val _key : string

type t = int * KindVersion.t list
(** The kind of a dance. For instance, [7x(32R + 64S + 128J)]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty version *)

val check : string -> bool

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
