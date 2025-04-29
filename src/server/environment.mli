(** {1 Environment}

    Persistent information on the user of Dancelor. This typically comprises
    sessions and “remember me” manipulations. *)

open Common

(** The abstract type of an environment. *)
type t

(** {2 Interface for the main server} *)

(** Processes a request to set the environment accordingly, then passes it to
    the given function, get the response that it creates, and processes that
    response.

    This will grab the session and “remember me” cookies from the request,
    restore the session if there is a recent-enough one (and create a new one
    otherwise), and possibly log in the user if it is not signed-in but has a
    “remember me” cookie. At the end, this will add the session cookie to the
    response, and possibly add or delete a “remember me” cookie as well. *)
val with_ :
  Cohttp.Request.t ->
  (t -> (Cohttp.Response.t * 'body) Lwt.t) ->
  (Cohttp.Response.t * 'body) Lwt.t

(** {2 Interface for controllers} *)

(** Returns the user that is signed-in in the current session. *)
val user : t -> Model.User.t Entry.t option

(** Set the user as signed in for the current session. Subsequent calls to
    {!user} (across requests) will return that user. If the [~remember_me] flag
    is set, then also set up the user to be remembered in the future, modifying
    the database and registering the appropriate response cookie. *)
val sign_in : t -> Model.User.t Entry.t -> remember_me: bool -> unit Lwt.t

(** Set the user as signed out for the current session. Subsequent calls to
    {!user} (across requests) will return [None]. Any “remember me” token will
    be erased from the database and the client's cookies. *)
val sign_out : t -> Model.User.t Entry.t -> unit Lwt.t

(** {2 Other} *)

val pp : Format.formatter -> t -> unit
