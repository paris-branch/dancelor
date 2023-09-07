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

(** {2 Filters} *)

type dance_kind = t
(** Alias for {!t} needed for the type interface of {!Filter}. *)

module Filter : sig
  type t [@@deriving yojson]

  val accepts : t -> dance_kind -> float Lwt.t

  val is : dance_kind -> t
  val base : KindBase.Filter.t -> t

  val raw : string -> t TextFormula.or_error
  val nullary_text_predicates : (string * t) list
  val unary_text_predicates : (string * (TextFormula.t -> t TextFormula.or_error)) list

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
end
