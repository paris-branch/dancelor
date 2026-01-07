module type S = sig
  (** {1 Set parameters}

      This module defines parameters that make sense at the level of a set. This
      includes version parameters as well. *)

  open Nes

  type t = Core.Set_parameters.t

  val make :
    ?display_name: NEString.t ->
    ?display_conceptor: NEString.t ->
    ?display_kind: NEString.t ->
    ?every_version: Core.Version_parameters.t ->
    unit ->
    t

  val equal : t -> t -> bool

  val none : t

  val display_name : t -> NEString.t option
  val display_conceptor : t -> NEString.t option
  val display_kind : t -> NEString.t option

  val every_version : t -> Core.Version_parameters.t

  val compose : t -> t -> t
end
