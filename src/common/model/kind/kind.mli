open Nes

(** {1 Kind} *)

module Base = KindBase
module Version = KindVersion

(** {2 Dance Kind} *)

type dance = int * KindVersion.t list
(** The kind of a dance. For instance, [7x(32R + 64S + 128J)]. *)

val dance_to_string : dance -> string
val dance_of_string : string -> dance
val dance_of_string_opt : string -> dance option

val dance_to_pretty_string : dance -> string
(** Pretty version *)

val check_dance : string -> bool

val dance_to_yojson : dance -> Json.t
val dance_of_yojson : Json.t -> (dance, string) result

module Dance : Madge_common.SERIALISABLE with type t = dance
