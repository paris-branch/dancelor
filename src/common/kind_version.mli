(** {1 Version Kind} *)

type t = int * Kind_base.t
[@@deriving eq, ord, show, yojson]
(** The kind of a version. For instance, [32R]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty t *)
