module type S = sig
  (** {1 Version} *)

  open Nes

  module Content : sig
    type part_name = Core.Version.Content.part_name

    type structure = (* Core.Version.Content.structure = *) part_name list

    val structure_to_string : structure -> string

    type part = Core.Version.Content.part = {
      melody: string; (** the melody of that part; they must not include clef or time; they may include the key *)
      chords: string; (** the chords of that part; they will be interpreted in LilyPond's [\chordmode] *)
      bars: int; (** how many bars does this part span; most often 8 bars *)
    }

    val lilypond_from_parts : (part_name * part) NEList.t -> string
    (** Combine the parts into a LilyPond string. *)

    type t = Core.Version.Content.t =
      | Monolithic of {lilypond: string; bars: int; structure: structure} (** A tune as a full LilyPond, including clef, key, etc. *)
      | Destructured of {parts: (part_name * part) NEList.t; common_structures: structure NEList.t} (** A tune decomposed as building blocks *)
    [@@deriving variants]

    val lilypond : t -> string
    (** Return the full immediately, or call {!lilypond_from_parts}. *)
  end

  type t = Core.Version.t

  val make :
    tune: Core.Tune.t Entry.t ->
    key: Music.key ->
    ?sources: (Core.Source.t Entry.t * Content.structure) list ->
    ?arrangers: Core.Person.t Entry.t list ->
    ?remark: string ->
    ?disambiguation: string ->
    content: Content.t ->
    unit ->
    t

  val tune : t -> Core.Tune.t Entry.t Lwt.t
  val tune' : t Entry.t -> Core.Tune.t Entry.t Lwt.t

  val key : t -> Music.key
  val key' : t Entry.t -> Music.key

  val sources : t -> (Core.Source.t Entry.t * Content.structure) list Lwt.t
  val sources' : t Entry.t -> (Core.Source.t Entry.t * Content.structure) list Lwt.t

  val arrangers : t -> Core.Person.t Entry.t list Lwt.t
  val arrangers' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val remark : t -> string
  val remark' : t Entry.t -> string

  val disambiguation : t -> string
  val disambiguation' : t Entry.t -> string

  val content : t -> Content.t
  val content' : t Entry.t -> Content.t

  val names : t -> NEString.t NEList.t Lwt.t
  val names' : t Entry.t -> NEString.t NEList.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.names}. *)

  val one_name : t -> NEString.t Lwt.t
  val one_name' : t Entry.t -> NEString.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.one_name}. *)

  val other_names : t -> NEString.t list Lwt.t
  val other_names' : t Entry.t -> NEString.t list Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.other_names}. *)

  val slug : t -> Entry.Slug.t Lwt.t
  val slug' : t Entry.t -> Entry.Slug.t Lwt.t

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  val set_content : Content.t -> t -> t

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t option Lwt.t
end
