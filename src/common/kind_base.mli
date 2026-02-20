open Nes

(** {1 Base Kind} *)

type t =
  Jig | Reel | Strathspey | Waltz | Polka | Other
[@@deriving eq, ord, show, yojson]

val all : t list

val to_short_string : t -> string
(** Short string, eg. ["S"]. *)

val to_long_string : capitalised: bool -> t -> string
(** Long string, eg. ["Strathspey"]. If [~capitalised], then the first letter is
    capitalised. *)

val of_string : string -> t
val of_string_opt : string -> t option

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

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t

  val optimise : t -> t
end
