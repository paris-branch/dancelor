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

(** Variant of {!call} that receives a continuation taking the result, instead
    of returning it. *)
val call_gen : ?retry: bool -> ('a, 'z Lwt.t, 'r) Route.t -> (('r, error) result -> 'z Lwt.t) -> 'a
