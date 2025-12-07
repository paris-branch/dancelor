(** {1 Madge — client-side} *)

open Nes

include module type of Madge

(** {2 Errors} *)

type error =
  | Http of {request: Request.t; status: Cohttp.Code.status_code; message: string}
  | ServerUnreachable of {request: Request.t; status: Cohttp.Code.status_code}
  | BodyUnserialisation of {body: string; message: string}

exception Error of error

(** {2 Endpoints} *)

(** Follow the route, call the endpoint, get the result and unserialise it, or
    returns an {!error}. *)
val call : ?retry: bool -> ('a, ('r, error) result Lwt.t, 'r) Route.t -> 'a

(** Variant of {!call} that raises {!Error} instead of returning it. *)
val call_exn : ?retry: bool -> ('a, 'r Lwt.t, 'r) Route.t -> 'a

(** Variant of {!call} that immediately receives a continuation taking the
    promise of a result. *)
val call_gen : ?retry: bool -> ('a, 'z, 'r) Route.t -> (('r, error) result Lwt.t -> 'z) -> 'a

(** {2 Other} *)

val initialise_batch_route : (Request.t list -> unit Lwt.t, unit Lwt.t, Response.t list) route -> unit
