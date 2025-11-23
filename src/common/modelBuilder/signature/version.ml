module type S = sig
  (** {1 Version} *)

  open Nes

  module Part_name : sig
    type t = Core.Version.Part_name.t

    val of_char : char -> t option
    val of_char_exn : char -> t
    val of_string : string -> t option

    val to_char : t -> char
    val to_string : t -> string

    type open_ = Core.Version.Part_name.open_ =
      Start | Middle of t | End
    [@@deriving eq, yojson, show {with_path = false}, variants]

    val open_to_string : open_ -> string
    val open_of_string : string -> open_ option
  end

  module Structure : sig
    type t = (* Core.Version.Structure.t = *) Part_name.t NEList.t

    val to_string : t -> NEString.t
    val of_string : NEString.t -> t option

    type folded_item = Core.Version.Structure.folded_item = Part of Part_name.t | Repeat of int * folded
    and folded = folded_item list

    val part : Part_name.t -> folded_item
    val repeat : int -> folded -> folded_item

    val first_part : folded -> Part_name.t option
    (** Find the first part in a structure, regarless of whether it appears in a
        repeat. *)

    val first_part_exn : folded -> Part_name.t
    (** Like {!first_part} but raises [Failure] if there is no first part. *)

    val last_part : folded -> Part_name.t option
    (** Find the last part in a structure, regarless of whether it appears in a
        repeat. *)

    val last_part_exn : folded -> Part_name.t
    (** Like {!last_part} but raises [Failure] if there is no last part. *)

    val best_fold_for : t -> folded option
    (** Given a (normal, flat) structure, compute the best possible fold for it,
        or return [None] if there is no good one. *)
  end

  module Voices : sig
    type t = Core.Version.Voices.t = {
      melody: string; (** the melody of that part; they must not include clef or time; they may include the key *)
      chords: string; (** the chords of that part; they will be interpreted in LilyPond's [\chordmode] *)
    }
    [@@deriving fields]

    val empty : t

    val space : t
    (** A space in both melody and chords. *)

    val section_break : t
    (** [\section\break] in the melody; space in the chords. *)

    val fine : t
    (** [\fine] in the melody; nothing in the chords. *)

    val mark : Part_name.t -> t
    (** A part name mark in the melody; nothing in the chords. *)

    val concat : t -> t -> t
    (** Concatenates melodies together and chords together in both voices. *)

    val concat_l : t list -> t
    (** Like {!concat} but on a list of voices. *)
  end

  module Content : sig
    type destructured = Core.Version.Content.destructured = {
      parts: Voices.t NEList.t;
      transitions: (Part_name.open_ * Part_name.open_ * Voices.t) list;
      default_structure: Structure.t;
    }

    type t = Core.Version.Content.t =
      | Monolithic of {lilypond: string; bars: int; structure: Structure.t} (** A tune as a full LilyPond, including clef, key, etc. *)
      | Destructured of destructured (** A tune decomposed as building blocks *)
    [@@deriving variants]
  end

  type source = Core.Version.source = {
    source: Core.Source.t Entry.t;
    structure: Structure.t;
    details: string;
  }

  type t = Core.Version.t

  val make :
    tune: Core.Tune.t Entry.t ->
    key: Music.Key.t ->
    ?sources: source list ->
    ?arrangers: Core.Person.t Entry.t list ->
    ?remark: string ->
    ?disambiguation: string ->
    content: Content.t ->
    unit ->
    t

  val tune : t -> Core.Tune.t Entry.t Lwt.t
  val tune' : t Entry.t -> Core.Tune.t Entry.t Lwt.t

  val key : t -> Music.Key.t
  val key' : t Entry.t -> Music.Key.t

  val sources : t -> source list Lwt.t
  val sources' : t Entry.t -> source list Lwt.t

  val arrangers : t -> Core.Person.t Entry.t list Lwt.t
  val arrangers' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val remark : t -> string
  val remark' : t Entry.t -> string

  val disambiguation : t -> string
  val disambiguation' : t Entry.t -> string

  val content : t -> Content.t
  val content' : t Entry.t -> Content.t
  (** Raises {!Failure} on the client side. *)

  val content_lilypond : ?content: Content.t -> t -> string Lwt.t
  val content_lilypond' : ?content: Content.t -> t Entry.t -> string Lwt.t
  (** Convenient wrapper around {!Content.lilypond} that grabs the right
      information from {!tune}. If the optional [?content] argument is not
      provided, the content is taken from the version with {!content} which
      raise an exception on the client side. *)

  val names : t -> NEString.t NEList.t Lwt.t
  val names' : t Entry.t -> NEString.t NEList.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.names}. *)

  val one_name : t -> NEString.t Lwt.t
  val one_name' : t Entry.t -> NEString.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.one_name}. *)

  val other_names : t -> NEString.t list Lwt.t
  val other_names' : t Entry.t -> NEString.t list Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.other_names}. *)

  val kind : t -> Kind.Base.t Lwt.t
  val kind' : t Entry.t -> Kind.Base.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.kind}. *)

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
