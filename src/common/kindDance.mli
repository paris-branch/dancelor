open Nes

(** {1 Dance Kind} *)

include module type of KindDanceType
(** The kind of a dance. For instance, [7x(32R + 2x64S + 128J)]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty version *)

val yojson_of_t : t -> Json.t
val t_of_yojson : Json.t -> t

val version_kinds : t -> KindVersion.t list
(** Returns the version kinds contained in the dance kind. For instance, for
    [2x32R + 8x(40R + 64J)], {!version_kinds} returns [\[32R; 40R; 64J\]]. *)

val to_simple : t -> (int * int * KindBase.t) option
(** If the dance kind contains only one base kind, returns it as a simple [N x M
    <base>]. For instance, [8x32J] returns [(8, 32, Jig)], and [32R + 8x40R +
    32R] returns [(1, 384, Reel)]. *)

(** {2 Filters} *)

module Filter : sig
  type predicate =
    | Is of t
    | Simple
    | Version of KindVersion.Filter.t

  val is : KindDanceType.t -> predicate
  val version : KindVersion.Filter.t -> predicate
  val simple : predicate

  val base : KindBase.Filter.t -> predicate
  val baseIs : KindBase.t -> predicate

  type t = predicate Formula.t
  [@@deriving eq, show, yojson]

  val is' : KindDanceType.t -> t
  val version' : KindVersion.Filter.t -> t
  val simple' : t

  val base' : KindBase.Filter.t -> t
  val baseIs' : KindBase.t -> t

  val accepts : t -> KindDanceType.t -> float Lwt.t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t

  val optimise : t -> t
end
