module type S = sig
  (** {1 Set parameters}
  
      This module defines parameters that make sense at the level of a set. This
      includes version parameters as well. *)

  open Nes

  (** How to render the order. [Default] prints the tunes as they appear in the
      set. [Unfolded] follows the order, duplicating the tunes if they are to be
      played several times. *)
  type order_type =
    Default | Unfolded

  type t = Core.SetParameters.t

  val make :
    ?forced_pages: int ->
    ?show_deviser: bool ->
    ?show_order: bool ->
    ?display_name: string ->
    ?for_dance: Core.Dance.t Entry.Id.t ->
    ?every_version: Core.VersionParameters.t ->
    unit ->
    t

  val make' :
    ?forced_pages: int ->
    ?show_deviser: bool ->
    ?show_order: bool ->
    ?display_name: string ->
    ?for_dance: Core.Dance.t Entry.t ->
    ?every_version: Core.VersionParameters.t ->
    unit ->
    t Lwt.t

  val none : t

  val for_dance : t -> Core.Dance.t Entry.t Olwt.t
  val show_order' : t -> bool
  val display_name : t -> string option
  val display_name' : default: string -> t -> string
  val show_deviser' : t -> bool
  val forced_pages' : t -> int
  val order_type' : t -> order_type

  val every_version : t -> Core.VersionParameters.t

  val set_show_order : bool -> t -> t
  (* FIXME: generic [update] *)

  val compose : t -> t -> t
end
