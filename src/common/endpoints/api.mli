open Nes

(** {2 Internal API} *)

(** The type of endpoints handled by Dancelor's API. *)
type (_, _, _) internal =
  | Source : ('a, 'w, 'r) Source.t -> ('a, 'w, 'r) internal
  | Person : ('a, 'w, 'r) Person.t -> ('a, 'w, 'r) internal
  | Book : ('a, 'w, 'r) Book.t -> ('a, 'w, 'r) internal
  | Version : ('a, 'w, 'r) Version.t -> ('a, 'w, 'r) internal
  | Dance : ('a, 'w, 'r) Dance.t -> ('a, 'w, 'r) internal
  | Set : ('a, 'w, 'r) Set.t -> ('a, 'w, 'r) internal
  | Tune : ('a, 'w, 'r) Tune.t -> ('a, 'w, 'r) internal
  | Any : ('a, 'w, 'r) Any.t -> ('a, 'w, 'r) internal
  | User : ('a, 'w, 'r) User.t -> ('a, 'w, 'r) internal
  | Job : ('a, 'w, 'r) Job.t -> ('a, 'w, 'r) internal
  | ReportIssue : (IssueReport.request -> 'w, 'w, IssueReport.response) internal
  | Victor : ('w, 'w, Void.t) internal
  | BootTime : ('w, 'w, Datetime.t) internal
  | Metrics : ('w, 'w, Void.t) internal

(** The type of wrapped endpoints. This allows manipulating the endpoints
    together, even though they do not have the same type. *)
type wrapped_internal = W_internal : ('a, 'r Lwt.t, 'r) internal -> wrapped_internal

(** The list of all endpoints handled by the API. *)
val all_internals : wrapped_internal list

(** {2 Full API} *)

(** The type of endpoints handled by Dancelor's API. *)
type (_, _, _) full =
  | Api : ('a, 'w, 'r) internal -> ('a, 'w, 'r) full

(** The type of wrapped endpoints. This allows manipulating the endpoints
    together, even though they do not have the same type. *)
type wrapped_full = W_full : ('a, 'r Lwt.t, 'r) full -> wrapped_full

(** {2 Internal API, lifted} *)

(** Return a string representing the endpoint. *)
val name : ('a, 'w, 'r) internal -> string

(** Return the Madge route corresponding to the given endpoint. *)
val route : ('a, 'w, 'r) internal -> ('a, 'w, 'r) Madge.route

(** Return the URL corresponding to the given endpoint. *)
val href : ('a, string, 'r) internal -> 'a
