module type S = sig
  (** {1 Source}

      This module represents a “source”, that is typically a publication
      containing tunes, and with which we can check the correspondance. *)

  open Nes

  type t = Core.Source.t
  (** Abstract type for a source. *)

  val make :
    name: string ->
    ?short_name: string ->
    ?editors: Core.Person.t Entry.t list ->
    ?scddb_id: int ->
    ?description: string ->
    unit ->
    t

  (** {2 Field getters} *)

  val name : t -> string
  val name' : t Entry.t -> string

  val short_name : t -> string
  val short_name' : t Entry.t -> string

  val editors : t -> Core.Person.t Entry.t list Lwt.t
  val editors' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val scddb_id : t -> int option
  val scddb_id' : t Entry.t -> int option

  val description : t -> string option
  val description' : t Entry.t -> string option

  val equal : t -> t -> bool

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t Lwt.t
end
