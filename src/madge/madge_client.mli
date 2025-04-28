(** {1 Madge — client-side} *)

open Nes

include module type of Madge

(** Error in an HTTP request. *)
type http_error = {
  request: Madge.Request.t;
  status: Cohttp.Code.status_code;
  message: string;
}

(** Follow the route, call the endpoint, get the result and unserialise it. If
    the call leads to an HTTP error, it is returned as an error. In case of
    other errors, they are raised. *)
val call : ('a, ('r, http_error) result Lwt.t, 'r) Route.t -> 'a

exception HttpError of http_error

(** Variant of {!call} that returns normally, or raises {!HttpError}. *)
val call_exn : ('a, 'r Lwt.t, 'r) Route.t -> 'a
