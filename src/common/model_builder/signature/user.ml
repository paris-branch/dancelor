module type S = sig
  (** {1 User} *)

  open Nes
  open Core

  type t = User.t

  type entry = t Entry.public

  module Password_reset_token_clear = User.Password_reset_token_clear
  module Password_reset_token_hashed = User.Password_reset_token_hashed

  val make :
    username: NEString.t ->
    ?password: HashedSecret.t ->
    ?password_reset_token: Password_reset_token_hashed.t * Datetime.t ->
    ?remember_me_tokens: (HashedSecret.t * Datetime.t) String.Map.t ->
    ?role: User.role ->
    unit ->
    t

  val update :
    ?username: (NEString.t -> NEString.t) ->
    ?password: (HashedSecret.t option -> HashedSecret.t option) ->
    ?password_reset_token: ((Password_reset_token_hashed.t * Datetime.t) option -> (Password_reset_token_hashed.t * Datetime.t) option) ->
    ?remember_me_tokens: ((HashedSecret.t * Datetime.t) String.Map.t -> (HashedSecret.t * Datetime.t) String.Map.t) ->
    ?role: (User.role -> User.role) ->
    t ->
    t Lwt.t

  (** {2 Field getters} *)

  val username : t -> NEString.t
  val username' : entry -> NEString.t

  val password : t -> HashedSecret.t option
  val password' : entry -> HashedSecret.t option

  val password_reset_token : t -> (Password_reset_token_hashed.t * Datetime.t) option
  val password_reset_token' : entry -> (Password_reset_token_hashed.t * Datetime.t) option

  val remember_me_tokens : t -> (HashedSecret.t * Datetime.t) String.Map.t
  val remember_me_tokens' : entry -> (HashedSecret.t * Datetime.t) String.Map.t

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
