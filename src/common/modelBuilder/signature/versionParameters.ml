module type S = sig
  (** {1 Version parameters}

      This module defines parameters that make sense at the level of a version. *)

  open Nes

  type t = Core.VersionParameters.t

  val make :
    ?instruments: string ->
    ?transposition: Transposition.t ->
    ?clef: Music.clef ->
    ?first_bar: int ->
    ?display_name: string ->
    ?display_composer: string ->
    ?for_dance: Core.Dance.t Entry.t ->
    unit ->
    t

  val none : t

  val for_dance : t -> Core.Dance.t Entry.t option Lwt.t
  val display_name : t -> string option
  val display_name' : default: string -> t -> string
  val display_composer' : default: string -> t -> string
  val clef : t -> Music.clef option
  val transposition' : t -> Transposition.t
  val make_instrument : Music.pitch -> t
  val trivia' : default: string -> t -> string
  val first_bar' : t -> int

  val set_display_name : string -> t -> t
  (* FIXME: generic [update] function *)

  val compose : t -> t -> t
end
