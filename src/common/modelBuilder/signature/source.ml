module type S = sig
  (** {1 Source}

      This module represents a “source”, that is typically a publication
      containing tunes, and with which we can check the correspondance. *)

  open Nes

  type t = Core.Source.t
  (** Abstract type for a source. *)

  type access = Entry.Access.public [@@deriving yojson]
  type entry = t Entry.public

  val make :
    name: NEString.t ->
    ?short_name: NEString.t ->
    ?editors: Core.Person.entry list ->
    ?scddb_id: int ->
    ?description: string ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val name : t -> NEString.t
  val name' : entry -> NEString.t

  val short_name : t -> NEString.t option
  val short_name' : entry -> NEString.t option

  val editors : t -> Core.Person.entry list Lwt.t
  val editors' : entry -> Core.Person.entry list Lwt.t

  val scddb_id : t -> int option
  val scddb_id' : entry -> int option

  val description : t -> string option
  val description' : entry -> string option

  val date : t -> PartialDate.t option
  val date' : entry -> PartialDate.t option

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> entry option Lwt.t
end
