(** {1 Rlwt}

    Helpers for the Result Lwt monad. *)

(** {2 Basic Monadic Interface} *)

type ('a, 'e) t = ('a, 'e) Result.t Lwt.t

val return : 'a -> ('a, 'e) t

val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

(** {3 Error Part of the Monad} *)

val fail : 'e -> ('a, 'e) t

val catch : ('a, 'e) t -> ('e -> ('a, 'f) t) -> ('a, 'f) t
