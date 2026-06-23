(** {1 Madge — client-side} *)

open Nes

include module type of Madge

(** {2 Errors} *)

type error =
  | Http of {request: Request.t; status: Cohttp.Code.status_code; message: string}
  | Server_unreachable of {request: Request.t; status: Cohttp.Code.status_code}
  | Body_unserialisation of {body: string; message: string}

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

(** Hook that is called when the server is reachable, that is when a
    connection could be established and it returns anything but a
    transient HTTP 5** error (eg. 503). *)
val on_server_reachable : (unit -> unit) ref

(** Hook that is called when the server is unreachable, that is when
    it returns an transient HTTP 5** error (eg. 503) or when the
    connection couldn't be established. Note that it is called at each
    retry and will therefore fire even if a subsequent retry would
    succeed. *)
val on_server_unreachable : (unit -> unit) ref
