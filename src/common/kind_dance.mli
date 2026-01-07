open Nes

(** {1 Dance Kind} *)

include module type of Kind_dance_type
(** The kind of a dance. For instance, [7x(32R + 2x64S + 128J)]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty version *)

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val to_simple : t -> (int * int * Kind_base.t) option
(** If the dance kind contains only one base kind, returns it as a simple [N x M
    <base>]. For instance, [8x32J] returns [(8, 32, Jig)], and [32R + 8x40R +
    32R] returns [(1, 384, Reel)]. *)

(** {2 Filters} *)

module Filter : sig
  type predicate =
    | Is of t
    | Simple
    | Version of Kind_version.Filter.t

  val is : Kind_dance_type.t -> predicate

  val base : Kind_base.Filter.t -> predicate
  val baseIs : Kind_base.t -> predicate

  type t = predicate Formula.t
  [@@deriving eq, show, yojson]

  val is' : Kind_dance_type.t -> t

  val base' : Kind_base.Filter.t -> t
  val baseIs' : Kind_base.t -> t

  val accepts : t -> Kind_dance_type.t -> float Lwt.t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t

  val optimise : t -> t
end
