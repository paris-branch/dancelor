module type S = sig
  (** {1 User} *)

  open Nes
  open Core

  type t = User.t

  val make :
    display_name: string ->
    person: Person.t Entry.t ->
    ?password: HashedSecret.t ->
    ?password_reset_token: HashedSecret.t * Datetime.t ->
    ?remember_me_tokens: (HashedSecret.t * Datetime.t) String.Map.t ->
    unit ->
    t

  val update :
    ?display_name: (string -> string) ->
    ?person: (Person.t Entry.t -> Person.t Entry.t Lwt.t) ->
    ?password: (HashedSecret.t option -> HashedSecret.t option) ->
    ?password_reset_token: ((HashedSecret.t * Datetime.t) option -> (HashedSecret.t * Datetime.t) option) ->
    ?remember_me_tokens: ((HashedSecret.t * Datetime.t) String.Map.t -> (HashedSecret.t * Datetime.t) String.Map.t) ->
    t ->
    t Lwt.t

  (** {2 Field getters} *)

  val display_name : t Entry.t -> string
  val person : t Entry.t -> Person.t Entry.t Lwt.t
  val password : t Entry.t -> HashedSecret.t option
  val password_reset_token : t Entry.t -> (HashedSecret.t * Datetime.t) option
  val remember_me_tokens : t Entry.t -> (HashedSecret.t * Datetime.t) String.Map.t
end
