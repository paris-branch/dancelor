(** {1 Environment}

    Client-side environment — persistent information on this run of Dancelor. *)

open Nes
open Common
open Html

(** Status of this run with respect to the server. It starts as {!Running} but
    might change to {!Offline} if the server does not answer, or {!Newer} if
    the server has reloaded. *)
type run_status = Running | Offline | Newer

val run_status : run_status S.t

val start_ping_routine : unit -> unit

val user : Model.User.t Entry.t option Lwt.t
