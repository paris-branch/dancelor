module type S = sig
  (** {1 Version parameters}

      This module defines parameters that make sense at the level of a version. *)

  open Nes

  type t = Core.Version_parameters.t

  val make :
    ?transposition: Transposition.t ->
    ?clef: Music.Clef.t ->
    ?structure: Core.Version.Structure.t ->
    ?first_bar: int ->
    ?display_name: NEString.t ->
    ?display_composer: NEString.t ->
    unit ->
    t

  val equal : t -> t -> bool

  val none : t

  val display_name : t -> NEString.t option
  val display_composer : t -> NEString.t option
  val clef : t -> Music.Clef.t option
  val structure : t -> Core.Version.Structure.t option
  val transposition : t -> Transposition.t option
  val trivia' : default: string -> t -> string

  val first_bar : t -> int option
  val first_bar' : t -> int

  val set_display_name : NEString.t -> t -> t
  (* FIXME: generic [update] function *)

  val compose : t -> t -> t
end
