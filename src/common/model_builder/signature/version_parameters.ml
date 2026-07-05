module type S = sig
  (** {1 Version parameters}

      This module defines parameters that make sense at the level of a version. *)

  open Nes

  type maybe_structure =
    | Force_no_structure
    | Structure of Core.Version.Structure.t

  val maybe_structure_to_string : maybe_structure -> NEString.t
  val maybe_structure_of_string : NEString.t -> maybe_structure option

  type t = Core.Version_parameters.t

  val make :
    ?transposition: Transposition.t ->
    ?clef: Music.Clef.t ->
    ?structure: maybe_structure ->
    ?first_bar: int ->
    ?trivia: string ->
    ?display_name: NEString.t ->
    ?display_composer: NEString.t ->
    unit ->
    t

  val equal : t -> t -> bool

  val none : t

  val display_name : t -> NEString.t option
  val display_composer : t -> NEString.t option
  val clef : t -> Music.Clef.t option
  val structure : t -> maybe_structure option
  val transposition : t -> Transposition.t option
  val trivia' : default: string -> t -> string

  val first_bar : t -> int option
  val first_bar' : t -> int

  val set_display_name : NEString.t -> t -> t
  (* FIXME: generic [update] function *)

  val compose : t -> t -> t
end
