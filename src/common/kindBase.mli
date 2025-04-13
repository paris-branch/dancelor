open Nes

(** {1 Base Kind} *)

type t =
  Jig | Polka | Reel | Strathspey | Waltz
[@@deriving eq, show]

val all : t list

val to_char : t -> char
val of_char : char -> t

val to_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option

val to_pretty_string : ?capitalised: bool -> t -> string
(** Pretty version. Capitalised if the corresponding boolean is set to true
    (default: false). *)

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val tempo : t -> string * int
(** Returns the base lilypond unit and the associated tempo. eg. [("2", 108)]
    for reels. *)

(** {2 Filters} *)

type base_kind = t
(** Alias for {!t} needed for the type interface of {!Filter}. *)

module Filter : sig
  type predicate = Is of t
  type t = predicate Formula.t
  [@@deriving eq, show, yojson]

  val accepts : t -> base_kind -> float Lwt.t

  val is : base_kind -> predicate
  val is' : base_kind -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t

  val optimise : t -> t
end
