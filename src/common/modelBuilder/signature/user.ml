module type S = sig
  (** {1 User} *)

  open Nes
  open Core

  type t = User.t

  val make :
    person: Person.t Entry.t ->
    ?password: HashedSecret.t ->
    ?password_reset_token: HashedSecret.t * Datetime.t ->
    ?remember_me_token: HashedSecret.t * Datetime.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val person : t Entry.t -> Person.t Entry.t Lwt.t
  val password : t Entry.t -> HashedSecret.t option
  val password_reset_token : t Entry.t -> (HashedSecret.t * Datetime.t) option
  val remember_me_token : t Entry.t -> (HashedSecret.t * Datetime.t) option
end
