open Nes

(** {1 Kind} *)

(** {2 Base Kind} *)

type base = Jig | Polka | Reel | Strathspey | Waltz

val base_to_char : base -> char
val base_of_char : char -> base

val base_to_string : base -> string
val base_of_string : string -> base

val pprint_base : base -> string

val base_to_yojson : base -> Json.t
val base_of_yojson : Json.t -> (base, string) result

(** {2 Version Kind} *)

type version = int * base
(** The kind of a version. For instance, [32R]. *)

val version_to_string : version -> string
val version_of_string : string -> version

val version_to_yojson : version -> Json.t
val version_of_yojson : Json.t -> (version, string) result

(** {2 Dance Kind} *)

type dance = int * version list
(** The kind of a dance. For instance, [7x(32R + 64S + 128J)]. *)

val dance_to_string : dance -> string
val dance_of_string : string -> dance

val check_dance : string -> bool

val dance_to_yojson : dance -> Json.t
val dance_of_yojson : Json.t -> (dance, string) result

module Dance : Madge_common.SERIALISABLE with type t = dance
