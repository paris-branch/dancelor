open Common

(** The abstract type of an environment. *)
type t

(** Processes a request to set the environment accordingly. *)
val make : request: Cohttp.Request.t -> t Lwt.t

(** Add cookies to a set of headers. This function is intended to be called when
    creating a response; it adds the appropriate cookies to the environment. *)
val add_cookies : t -> Cohttp.Header.t -> Cohttp.Header.t

val user : t -> Model.User.t Entry.t option

val set_user_and_remember_me : t -> Model.User.t Entry.t -> bool -> unit Lwt.t

val unset_user_and_remember_me : t -> Model.User.t Entry.t -> unit Lwt.t
