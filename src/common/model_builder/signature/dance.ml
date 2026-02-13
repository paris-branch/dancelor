module type S = sig
  (** {1 Dance} *)

  open Nes

  type two_chords = Core.Dance.two_chords =
    | Dont_know
    | One_chord
    | Two_chords

  type t = Core.Dance.t

  type access = Entry.Access.public [@@deriving yojson]
  type entry = t Entry.public

  val make :
    names: NEString.t NEList.t ->
    kind: Kind.Dance.t ->
    ?devisers: Core.Person.entry list ->
    ?two_chords: two_chords ->
    ?scddb_id: int ->
    ?disambiguation: string ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val names : t -> NEString.t NEList.t
  val names' : entry -> NEString.t NEList.t

  val one_name : t -> NEString.t
  val one_name' : entry -> NEString.t
  val other_names : t -> NEString.t list
  val other_names' : entry -> NEString.t list

  val kind : t -> Kind.Dance.t
  val kind' : entry -> Kind.Dance.t

  val devisers : t -> Core.Person.entry list Lwt.t
  val devisers' : entry -> Core.Person.entry list Lwt.t

  val two_chords : t -> two_chords
  val two_chords' : entry -> two_chords

  val scddb_id : t -> int option
  val scddb_id' : entry -> int option

  val disambiguation : t -> string
  val disambiguation' : entry -> string

  val date : t -> PartialDate.t option
  val date' : entry -> PartialDate.t option

  val slug : t -> NesSlug.t
  val slug' : entry -> NesSlug.t

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> entry option Lwt.t
end
