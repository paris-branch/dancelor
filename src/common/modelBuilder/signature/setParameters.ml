module type S = sig
  (** {1 Set parameters}

      This module defines parameters that make sense at the level of a set. This
      includes version parameters as well. *)

  open Nes

  type t = Core.SetParameters.t

  val make :
    ?display_name: NEString.t ->
    ?display_conceptor: NEString.t ->
    ?display_kind: NEString.t ->
    ?every_version: Core.VersionParameters.t ->
    unit ->
    t

  val equal : t -> t -> bool

  val none : t

  val display_name : t -> NEString.t option
  val display_conceptor : t -> NEString.t option
  val display_kind : t -> NEString.t option

  val every_version : t -> Core.VersionParameters.t

  val compose : t -> t -> t
end
