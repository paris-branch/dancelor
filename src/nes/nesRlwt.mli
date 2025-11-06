(** {1 Rlwt}

    Helpers for the Result Lwt monad. *)

(** {2 Basic Monadic Interface} *)

type ('a, 'e) t = ('a, 'e) Result.t Lwt.t

val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
