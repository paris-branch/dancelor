module type S = sig
  (** {1 User} *)

  open Nes
  open Core

  type t = User.t

  val make :
    username: NEString.t ->
    ?password: HashedSecret.t ->
    ?password_reset_token: HashedSecret.t * Datetime.t ->
    ?remember_me_tokens: (HashedSecret.t * Datetime.t) String.Map.t ->
    unit ->
    t

  val update :
    ?username: (NEString.t -> NEString.t) ->
    ?password: (HashedSecret.t option -> HashedSecret.t option) ->
    ?password_reset_token: ((HashedSecret.t * Datetime.t) option -> (HashedSecret.t * Datetime.t) option) ->
    ?remember_me_tokens: ((HashedSecret.t * Datetime.t) String.Map.t -> (HashedSecret.t * Datetime.t) String.Map.t) ->
    t ->
    t Lwt.t

  (** {2 Field getters} *)

  val username : t -> NEString.t
  val username' : t Entry.t -> NEString.t

  val password : t -> HashedSecret.t option
  val password' : t Entry.t -> HashedSecret.t option

  val password_reset_token : t -> (HashedSecret.t * Datetime.t) option
  val password_reset_token' : t Entry.t -> (HashedSecret.t * Datetime.t) option

  val remember_me_tokens : t -> (HashedSecret.t * Datetime.t) String.Map.t
  val remember_me_tokens' : t Entry.t -> (HashedSecret.t * Datetime.t) String.Map.t

  val admin : t Entry.t -> bool
end
