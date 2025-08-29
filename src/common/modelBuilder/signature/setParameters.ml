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
    ?display_name: NEString.t ->
    ?display_conceptor: string ->
    ?display_kind: Kind.Dance.t ->
    ?every_version: Core.VersionParameters.t ->
    unit ->
    t

  val equal : t -> t -> bool

  val none : t

  val show_order' : t -> bool
  val display_name : t -> NEString.t option
  val display_conceptor : t -> string option
  val display_kind : t -> Kind.Dance.t option
  val show_deviser' : t -> bool
  val forced_pages' : t -> int
  val order_type' : t -> order_type

  val every_version : t -> Core.VersionParameters.t

  val set_show_order : bool -> t -> t
  (* FIXME: generic [update] *)

  val compose : t -> t -> t
end
