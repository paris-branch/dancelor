(** {1 Olwt}

    Helpers for the Option Lwt monad. *)

(** {2 Basic Monadic Interface} *)

type 'a t = 'a Option.t Lwt.t

val return : 'a -> 'a t

val bind : 'a t -> ( 'a -> 'b t) -> 'b t

(** {3 Error Part of the Monad} *)

val fail : unit -> 'a t

val catch : 'a t -> (unit -> 'a t) -> 'a t
