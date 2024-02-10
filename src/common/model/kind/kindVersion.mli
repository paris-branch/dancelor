open Nes

(** {1 Version Kind} *)

val _key : string

type t = int * KindBase.t
[@@deriving eq, show]
(** The kind of a version. For instance, [32R]. *)

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : t -> string
(** Pretty t *)

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val gen : t QCheck.Gen.t
val shrink : t QCheck.Shrink.t

(** {2 Filters} *)

type version_kind = t
(** Alias for {!t} needed for the type interface of {!Filter}. *)

module Filter : sig
  type predicate
  type t = predicate Formula.t
  [@@deriving eq, show, yojson]

  val accepts : t -> version_kind -> float Lwt.t

  val is : version_kind -> predicate
  val is' : version_kind -> t

  val base : KindBase.Filter.t -> predicate
  val base' : KindBase.Filter.t -> t

  val barsEq : int -> predicate
  val barsEq' : int -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename:string -> string -> (t, string) Result.t

  val gen : t QCheck.Gen.t
end
