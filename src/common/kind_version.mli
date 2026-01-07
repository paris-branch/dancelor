open Nes

(** {1 Version Kind} *)

type t = int * Kind_base.t
[@@deriving eq, show]
(** The kind of a version. For instance, [32R]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty t *)

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

(** {2 Filters} *)

type version_kind = t
(** Alias for {!t} needed for the type interface of {!Filter}. *)

module Filter : sig
  type predicate =
    | Is of t
    | Bars_eq of int
    | Bars_ne of int
    | Bars_gt of int
    | Bars_ge of int
    | Bars_lt of int
    | Bars_le of int
    | Base of Kind_base.Filter.t

  val is : version_kind -> predicate
  val base : Kind_base.Filter.t -> predicate

  val baseIs : Kind_base.t -> predicate

  type t = predicate Formula.t
  [@@deriving eq, show, yojson]

  val accepts : t -> version_kind -> float Lwt.t

  val is' : version_kind -> t
  val base' : Kind_base.Filter.t -> t

  val baseIs' : Kind_base.t -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t

  val optimise : t -> t
end
