(** {1 Olwt}

    Helpers for the Option Lwt monad. *)

(** {2 Basic Monadic Interface} *)

type 'a t = 'a option Lwt.t

val bind : 'a t -> ('a -> 'b t) -> 'b t
