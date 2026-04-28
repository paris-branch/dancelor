(** {1 User} *)

open Nes
open Dancelor_common

(** {2 Components} *)

module Password_hashed : Fresh.T with type base = HashedSecret.t
module Password_reset_token_hashed : Fresh.T with type base = HashedSecret.t

module Remember_me_key : sig
  include Fresh.T with type base = string
  module Map : sig
    include Map.S with type key = t
  end
end

module Remember_me_token_clear : Fresh.T with type base = string
module Remember_me_token_hashed : Fresh.T with type base = HashedSecret.t

(** {2 User} *)

type t
type entry = t Entry.public

val id_of_common : Entry.User.t Entry.id -> t Entry.id
val id_to_common : t Entry.id -> Entry.User.t Entry.id

val to_common : t -> Entry.User.t
val entry_to_common : entry -> Entry.User.t Entry.public

val username : t -> Username.t
val password : t -> Password_hashed.t option
val password_reset_token : t -> (Password_reset_token_hashed.t * Datetime.t) option
val remember_me_tokens : t -> (Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t
val role : t -> Entry.User.role

(** {2 Queries} *)

val get : t Entry.id -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val get_from_username : Username.t -> entry option Lwt.t

val create :
  username: Username.t ->
  role: Entry.User.role ->
  password_reset_token_hash: Password_reset_token_hashed.t ->
  password_reset_token_max_date: Datetime.t ->
  entry Lwt.t

val update : t Entry.id -> t -> entry Lwt.t

val delete : t Entry.id -> unit Lwt.t

val set_password_reset_token : t Entry.id -> Password_reset_token_hashed.t -> Datetime.t -> unit Lwt.t
(** For the given user, set the password reset token and max date, and clear the
    password and remember me tokens. *)

val set_password : t Entry.id -> Password_hashed.t -> unit Lwt.t
(** For the given user, set the password and clear the password reset token. *)

val find_remember_me_token : entry -> Remember_me_key.t -> (Remember_me_token_hashed.t * Datetime.t) option
val add_remember_me_token : entry -> Remember_me_key.t -> Remember_me_token_hashed.t -> Datetime.t -> unit Lwt.t
val remove_one_remember_me_token : entry -> Remember_me_key.t -> unit Lwt.t
val remove_all_remember_me_tokens : entry -> unit Lwt.t

val set_omniscience : entry -> bool -> unit Lwt.t
(** For the given user, set omniscience to the given boolean. *)
