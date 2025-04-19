open Common

(** The abstract type of an environment. *)
type t

val user : t -> Model.User.t Entry.t option

(** Processes a request to set the environment accordingly. *)
val make : request: Cohttp.Request.t -> t

(** Add cookies to a set of headers. This function is intended to be called when
    creating a response; it adds the appropriate cookies to the environment. *)
val add_cookies : t -> Cohttp.Header.t -> Cohttp.Header.t

(** Changes the user in the environment. *)
val set_user : t -> Model.User.t Entry.t option -> t
