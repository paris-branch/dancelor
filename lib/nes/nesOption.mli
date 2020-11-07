type 'a t = 'a option

val bind : 'a t -> ('a -> 'b t) -> 'b t
val compose : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
val map : ('a -> 'b) -> ('a t -> 'b t)

val wrap : 'a -> 'a t (* aka pure or return *)

val unwrap : 'a t -> 'a
val unwrap_or : default:'a -> 'a t -> 'a

val unwrap_map_or : default:'b -> ('a -> 'b) -> 'a t -> 'b

val wrap_fun : ('a -> 'b) -> ('a -> 'b t)

val ifsome : ('a -> unit) -> ('a t -> unit)
val ifsome_lwt : ('a -> unit Lwt.t) -> ('a t -> unit Lwt.t)

val choose : tie:('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
(** [choose ~tie first second] returns [Some x] when one of [first] or [second]
   is [Some x] and the other is bottom. If they are both [None], [None] is
   returned. If they are both [Some], [tie] is called. *)

val choose_strict : 'a t -> 'a t -> 'a t
(** Same as [choose] except it fails in case of tie. *)

val choose_latest : 'a t -> 'a t -> 'a t
(** Same as [choose] except it chooses the second one in case of tie. *)

val assert_some : 'a t -> 'a t
(** Fails when = None *)

val assert_ : bool -> unit t
(** [None] if [false], [Some ()] if [true]. *)

module Syntax : sig
  val (>>=?) : 'a t -> ('a -> 'b t) -> 'b t
  val (>=>?) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
end
