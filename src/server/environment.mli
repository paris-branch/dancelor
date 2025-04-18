open Common

type session

val user : session -> Model.User.t Entry.t option

(** The abstract type of an environment. *)
type t

val session : t -> session

val make : request: Cohttp.Request.t -> unit -> t

val add_session_cookie : t -> Cohttp.Header.t -> Cohttp.Header.t

(** Changes the session user in the environment. *)
val set_session_user : t -> Model.User.t Entry.t option -> t
