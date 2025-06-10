(** {1 Madge — client-side} *)

open Nes

include module type of Madge

(** Error in an HTTP request. *)
type http_error = {
  request: Request.t;
  status: Cohttp.Code.status_code;
  message: string;
}

exception ServerUnreachable of Request.t
exception BodyUnserialisationError of string

(** Follow the route, call the endpoint, get the result and unserialise it. If
    the call leads to an HTTP error, it is returned as an error. May raise
    {!ServerUnreachable}, or {!BodyUnserialisationError}. *)
val call : ?retry: bool -> ('a, ('r, http_error) result Lwt.t, 'r) Route.t -> 'a

exception HttpError of http_error

(** Variant of {!call} that raises {!HttpError} in addition to the other
    exceptions. *)
val call_exn : ?retry: bool -> ('a, 'r Lwt.t, 'r) Route.t -> 'a
