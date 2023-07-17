open Nes

(** {1 Kind} *)

module Base = KindBase

(** {2 Version Kind} *)

type version = int * Base.t
(** The kind of a version. For instance, [32R]. *)

val version_to_string : version -> string
val version_of_string : string -> version
val version_of_string_opt : string -> version option

val version_to_pretty_string : version -> string
(** Pretty version *)

val version_to_yojson : version -> Json.t
val version_of_yojson : Json.t -> (version, string) result

module Version : Madge_common.SERIALISABLE with type t = version

(** {2 Dance Kind} *)

type dance = int * version list
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
