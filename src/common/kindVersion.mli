open Nes

(** {1 Version Kind} *)

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

(** {2 Filters} *)

type version_kind = t
(** Alias for {!t} needed for the type interface of {!Filter}. *)

module Filter : sig
  type predicate =
    | Is of t
    | BarsEq of int
    | BarsNe of int
    | BarsGt of int
    | BarsGe of int
    | BarsLt of int
    | BarsLe of int
    | Base of KindBase.Filter.t

  val is : version_kind -> predicate
  val base : KindBase.Filter.t -> predicate
  val barseq : int -> predicate
  val barsne : int -> predicate
  val barsgt : int -> predicate
  val barsge : int -> predicate
  val barslt : int -> predicate
  val barsle : int -> predicate

  val baseIs : KindBase.t -> predicate

  type t = predicate Formula.t
  [@@deriving eq, show, yojson]

  val accepts : t -> version_kind -> float Lwt.t

  val is' : version_kind -> t
  val base' : KindBase.Filter.t -> t
  val barsEq' : int -> t
  val barsNe' : int -> t
  val barsGt' : int -> t
  val barsGe' : int -> t
  val barsLt' : int -> t
  val barsLe' : int -> t

  val baseIs' : KindBase.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t

  val optimise : t -> t
end
