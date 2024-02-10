open Nes

(** {1 Dance Kind} *)

val _key : string

include module type of KindDanceType
(** The kind of a dance. For instance, [7x(32R + 2x64S + 128J)]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty version *)

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val version_kinds : t -> KindVersion.t list
(** Returns the version kinds contained in the dance kind. For instance, for
    [2x32R + 8x(40R + 64J)], {!version_kinds} returns [\[32R; 40R; 64J\]]. *)

(** {2 Filters} *)

module Filter : sig
  type predicate
  type t = predicate Formula.t
  [@@deriving show, yojson]

  val accepts : t -> KindDanceType.t -> float Lwt.t

  val is : KindDanceType.t -> predicate
  val is' : KindDanceType.t -> t

  val version : KindVersion.Filter.t -> predicate
  val version' : KindVersion.Filter.t -> t

  val simple : predicate
  val simple' : t

  val base : KindBase.Filter.t -> predicate
  val base' : KindBase.Filter.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename:string -> string -> (t, string) Result.t

  val gen : t QCheck.Gen.t
end
