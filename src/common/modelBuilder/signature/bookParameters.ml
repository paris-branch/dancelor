module type S = sig
  (** {1 Book parameters}

      This module defines parameters that make sense at the level of a book. This
      includes set parameters (which include version parameters) as well. *)

  type t = Core.BookParameters.t

  val make :
    ?simple: bool ->
    ?every_set: Core.SetParameters.t ->
    unit ->
    t

  val none : t

  val simple : t -> bool option
  (** FIXME: Find a better name for this option. *)

  val every_set : t -> Core.SetParameters.t

  val compose : t -> t -> t
end
