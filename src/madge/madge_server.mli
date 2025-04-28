(** {1 Madge — server-side} *)

open Nes

include module type of Madge

(** {2 Normal behaviour} *)

(** The main function of this module. Given a route, a controller and a request,
    attempt to match the request to the route, and, if it succeeds, apply the
    controller. The result of the controller is serialised as the route commands
    and returned as JSON under the form of an Lwt promise compatible with the
    [~callback] argument of {!Cohttp_unix_lwt.Server.make}. *)
val match_apply :
  ('a, 'r Lwt.t, 'r) Route.t ->
  (unit -> 'a) ->
  Request.t ->
  (unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) option

(** {2 Successful non-JSON responses}

    All of these return {Void.t}, meaning they can only ever be used on a route
    that is never returning normally anyway; otherwise, this would allow
    returning non-JSON on normal routes that expect it. *)

(** Returns the given string. *)
val respond_string : content_type: string -> string -> Void.t Lwt.t

(** Returns the given file. *)
val respond_file : content_type: string -> fname: string -> Void.t Lwt.t

(** {2 Error responses} *)

(** Shortcut execution and returns “400 Bad Request” with the given message. *)
val respond_bad_request : string -> 'any Lwt.t

(** Shortcut execution and returns “403 Forbidden” with the given message. *)
val respond_forbidden : string -> 'any Lwt.t

(** Variant of {!respond_forbidden} for when one purposefully does not want to
    give any information of why that was, typically for user authentication. *)
val respond_forbidden_no_leak : unit -> 'any Lwt.t

(** Shortcut execution and returns “404 Not Found” with the given message. *)
val respond_not_found : string -> 'any Lwt.t
