module type S = sig
  (** {1 User} *)

  open Nes
  open Core

  type t = User.t

  type entry = t Entry.public

  module Username = User.Username

  module Password_clear = User.Password_clear
  module Password_hashed = User.Password_hashed

  module Password_reset_token_clear = User.Password_reset_token_clear
  module Password_reset_token_hashed = User.Password_reset_token_hashed

  module Remember_me_key = User.Remember_me_key
  module Remember_me_token_clear = User.Remember_me_token_clear
  module Remember_me_token_hashed = User.Remember_me_token_hashed

  val make :
    username: Username.t ->
    ?password: Password_hashed.t ->
    ?password_reset_token: Password_reset_token_hashed.t * Datetime.t ->
    ?remember_me_tokens: (Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t ->
    ?role: User.role ->
    unit ->
    t

  val update :
    ?username: (Username.t -> Username.t) ->
    ?password: (Password_hashed.t option -> Password_hashed.t option) ->
    ?password_reset_token: ((Password_reset_token_hashed.t * Datetime.t) option -> (Password_reset_token_hashed.t * Datetime.t) option) ->
    ?remember_me_tokens: ((Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t -> (Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t) ->
    ?role: (User.role -> User.role) ->
    t ->
    t Lwt.t

  (** {2 Field getters} *)

  val username : t -> Username.t
  val username' : entry -> Username.t

  val password : t -> Password_hashed.t option
  val password' : entry -> Password_hashed.t option

  val password_reset_token : t -> (Password_reset_token_hashed.t * Datetime.t) option
  val password_reset_token' : entry -> (Password_reset_token_hashed.t * Datetime.t) option

  val remember_me_tokens : t -> (Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t
  val remember_me_tokens' : entry -> (Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t

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
