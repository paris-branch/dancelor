module type S = sig
  (** {1 User} *)

  open Nes
  open Core

  type t = User.t

  type entry = t Entry.public

  module Password_clear = User.Password_clear
  module Password_reset_token_clear = User.Password_reset_token_clear

  val make :
    username: Username.t ->
    ?role: User.role ->
    unit ->
    t

  val update :
    ?username: (Username.t -> Username.t) ->
    ?role: (User.role -> User.role) ->
    t ->
    t Lwt.t

  (** {2 Field getters} *)

  val username : t -> Username.t
  val username' : entry -> Username.t

  val role : t -> User.role
  val role' : entry -> User.role

  (** Whether the user is a database maintainer. *)
  val is_maintainer : t -> bool
  val is_maintainer' : entry -> bool

  (** Whether the user is an administrator. *)
  val is_administrator : t -> bool
  val is_administrator' : entry -> bool

  (** Whether the user is an administrator that enabled omniscience. *)
  val is_omniscient_administrator : t -> bool
  val is_omniscient_administrator' : entry -> bool

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> entry option Lwt.t
end
