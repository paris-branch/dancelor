module type S = sig
  (** {1 Book parameters}
  
      This module defines parameters that make sense at the level of a book. This
      includes set parameters (which include version parameters) as well. *)

  type where =
    Beginning | End | Nowhere

  type t = Core.BookParameters.t

  val make :
    ?front_page: bool ->
    ?table_of_contents: where ->
    ?two_sided: bool ->
    ?every_set: Core.SetParameters.t ->
    ?running_header: bool ->
    unit ->
    t

  val none : t

  val instruments' : t -> string
  val two_sided' : t -> bool
  val running_header' : t -> bool
  val running_footer' : t -> bool
  val front_page' : t -> bool
  val table_of_contents' : t -> where

  val every_set : t -> Core.SetParameters.t

  val compose : t -> t -> t
end
