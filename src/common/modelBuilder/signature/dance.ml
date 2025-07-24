module type S = sig
  (** {1 Dance} *)

  open Nes

  type t = Core.Dance.t

  val make :
    name: string ->
    kind: Kind.Dance.t ->
    ?devisers: Core.Person.t Entry.t list ->
    ?two_chords: bool ->
    ?scddb_id: int ->
    ?disambiguation: string ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val name : t -> string
  val name' : t Entry.t -> string

  val kind : t -> Kind.Dance.t
  val kind' : t Entry.t -> Kind.Dance.t

  val devisers : t -> Core.Person.t Entry.t list Lwt.t
  val devisers' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val two_chords : t -> bool option
  val two_chords' : t Entry.t -> bool option

  val scddb_id : t -> int option
  val scddb_id' : t Entry.t -> int option

  val disambiguation : t -> string
  val disambiguation' : t Entry.t -> string

  val date : t -> PartialDate.t option
  val date' : t Entry.t -> PartialDate.t option

  val equal : t Entry.t -> t Entry.t -> bool

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t Lwt.t
end
