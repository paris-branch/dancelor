(** {1 Dance Kind} *)

include module type of Kind_dance_type
(** The kind of a dance. For instance, [7x(32R + 2x64S + 128J)]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty version *)

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

val to_simple : t -> (int * int * Kind_base.t) option
(** If the dance kind contains only one base kind, returns it as a simple [N x M
    <base>]. For instance, [8x32J] returns [(8, 32, Jig)], and [32R + 8x40R +
    32R] returns [(1, 384, Reel)]. *)
