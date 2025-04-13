module type S = sig
  (** {1 User}

      This module represents a “user”, in the wide definition. A user can be
      an actual human user (eg. “William Marshall”), a group of people (eg. “The
      Craigellachie Band”) or an abstract notion (eg. “Traditional” or “RSCDS”).
      This is a notion similar to that of the SCDDB. *)

  open Nes
  open Core

  type t = User.t
  (** Abstract type for a user. *)

  val make : name: string -> person: Person.t Entry.t -> unit -> t

  (** {2 Field getters} *)

  val name : t Entry.t -> string
  val person : t Entry.t -> Person.t Entry.t Lwt.t

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Entry.t Lwt.t
  (** Look up a user in the database given its slug. On the client-side, this
      involves an API call. *)
end
