open Nes

(** The type of endpoints handled by Dancelor's API. *)
type (_, _, _) t =
  | Source : ('a, 'w, 'r) Source.t -> ('a, 'w, 'r) t
  | Person : ('a, 'w, 'r) Person.t -> ('a, 'w, 'r) t
  | Book : ('a, 'w, 'r) Book.t -> ('a, 'w, 'r) t
  | Version : ('a, 'w, 'r) Version.t -> ('a, 'w, 'r) t
  | Dance : ('a, 'w, 'r) Dance.t -> ('a, 'w, 'r) t
  | Set : ('a, 'w, 'r) Set.t -> ('a, 'w, 'r) t
  | Tune : ('a, 'w, 'r) Tune.t -> ('a, 'w, 'r) t
  | Any : ('a, 'w, 'r) Any.t -> ('a, 'w, 'r) t
  | User : ('a, 'w, 'r) User.t -> ('a, 'w, 'r) t
  | Job : ('a, 'w, 'r) Job.t -> ('a, 'w, 'r) t
  | ReportIssue : (IssueReport.request -> 'w, 'w, IssueReport.response) t
  | Victor : ('w, 'w, Void.t) t
  | BootTime : ('w, 'w, Datetime.t) t
  | Metrics : ('w, 'w, Void.t) t

(** Return a string representing the endpoint. *)
val name : ('a, 'w, 'r) t -> string

(** Return the Madge route corresponding to the given endpoint. *)
val route : ('a, 'w, 'r) t -> ('a, 'w, 'r) Madge.route

(** Return the URL corresponding to the given endpoint. *)
val href : ('a, string, 'r) t -> 'a

(** The type of wrapped endpoints. This allows manipulating the endpoints
    together, even though they do not have the same type. *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped

(** The list of all endpoints handled by the API. *)
val all : wrapped list
