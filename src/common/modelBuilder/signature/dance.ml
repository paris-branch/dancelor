module type S = sig
  (** {1 Dance} *)

  open Nes

  type t = Core.Dance.t

  val make :
    names: NEString.t NEList.t ->
    kind: Kind.Dance.t ->
    ?devisers: Core.Person.t Entry.t list ->
    ?two_chords: bool ->
    ?scddb_id: int ->
    ?disambiguation: string ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val names : t -> NEString.t NEList.t
  val names' : t Entry.t -> NEString.t NEList.t

  val one_name : t -> NEString.t
  val one_name' : t Entry.t -> NEString.t
  val other_names : t -> NEString.t list
  val other_names' : t Entry.t -> NEString.t list

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

  val slug : t -> Entry.Slug.t
  val slug' : t Entry.t -> Entry.Slug.t

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t option Lwt.t
end
