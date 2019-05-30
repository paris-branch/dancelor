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

(** {2 Tune Kind} *)

type tune = int * base
(** The kind of a tune. For instance, [32R]. *)

val tune_to_string : tune -> string
val tune_of_string : string -> tune

val tune_to_yojson : tune -> Json.t
val tune_of_yojson : Json.t -> (tune, string) result

(** {2 Dance Kind} *)

type dance = int * tune list
(** The kind of a dance. For instance, [7x(32R + 64S + 128J)]. *)

val dance_to_string : dance -> string
val dance_of_string : string -> dance

val dance_to_yojson : dance -> Json.t
val dance_of_yojson : Json.t -> (dance, string) result
