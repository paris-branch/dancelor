open Dancelor_common

(** {1 Kind} *)

(** {2 Base Kind} *)

type base = Jig | Polka | Reel | Strathspey | Waltz

val base_to_char : base -> char
val base_of_char : char -> base

val base_to_string : base -> string
val base_of_string : string -> base

val base_to_jsonm : base -> Json.value
val base_of_jsonm : Json.value -> base

(** {2 Tune Kind} *)

type tune = int * base
(** The kind of a tune. For instance, [32R]. *)

val tune_to_string : tune -> string
val tune_of_string : string -> tune

val tune_to_jsonm : tune -> Json.value
val tune_of_jsonm : Json.value -> tune

(** {2 Dance Kind} *)

type dance = int * tune list
(** The kind of a dance. For instance, [7x(32R + 64S + 128J)]. *)

val dance_to_string : dance -> string
val dance_of_string : string -> dance

val dance_to_jsonm : dance -> Json.value
val dance_of_jsonm : Json.value -> dance
