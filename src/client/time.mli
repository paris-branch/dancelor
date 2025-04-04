(* FIXME: Move to Nes *)

open React

val now : unit -> float
(** Alias for {!Unix.gettimeofday}. *)

val now_s : float S.t
(** Same as {!now} but as a signal. *)

val duration_human : int -> string
(** Given a duration in seconds, returns a string representing that duration in
    human terms, eg. ["37 minutes ago"]. *)

val ago : float -> string
(** Given a reference time, returns a string representing how far in the past it
    is, eg. ["37 minutes ago"]. *)

val ago_s : float -> string S.t
(** Same as {!ago} but as a signal. *)
