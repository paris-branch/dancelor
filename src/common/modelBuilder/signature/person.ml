module type S = sig
  (** {1 Person}

      This module represents a “person”, in the wide definition. A person can be
      an actual human person (eg. “William Marshall”), a group of people (eg. “The
      Craigellachie Band”) or an abstract notion (eg. “Traditional” or “RSCDS”).
      This is a notion similar to that of the SCDDB. *)

  open Nes

  type t = Core.Person.t
  (** Abstract type for a person. *)

  val make :
    name: NEString.t ->
    ?user: Core.User.t Entry.t ->
    ?scddb_id: int ->
    ?composed_tunes_are_public: bool ->
    ?published_tunes_are_public: bool ->
    unit ->
    t

  (** {2 Field getters} *)

  val name : t -> NEString.t
  val name' : t Entry.t -> NEString.t

  val user : t -> Core.User.t Entry.t option Lwt.t
  val user' : t Entry.t -> Core.User.t Entry.t option Lwt.t

  val scddb_id : t -> int option
  val scddb_id' : t Entry.t -> int option

  val composed_tunes_are_public : t -> bool
  val composed_tunes_are_public' : t Entry.t -> bool

  val published_tunes_are_public : t -> bool
  val published_tunes_are_public' : t Entry.t -> bool

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t option Lwt.t
end
