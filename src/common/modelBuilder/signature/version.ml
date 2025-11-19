module type S = sig
  (** {1 Version} *)

  open Nes

  module Content : sig
    module Part_name : sig
      type t = Core.Version.Content.Part_name.t

      val of_char : char -> t option
      val of_char_exn : char -> t
      val of_string : string -> t option

      val to_char : t -> char
      val to_string : t -> string

      type open_ = Core.Version.Content.Part_name.open_ =
        Start | Middle of t | End
      [@@deriving eq, yojson, show {with_path = false}, variants]

      val open_to_string : open_ -> string
      val open_of_string : string -> open_ option
    end

    type structure = (* Core.Version.Content.structure = *) Part_name.t NEList.t

    val structure_to_string : structure -> NEString.t
    val structure_of_string : NEString.t -> structure option

    type part = Core.Version.Content.part = {
      melody: string; (** the melody of that part; they must not include clef or time; they may include the key *)
      chords: string; (** the chords of that part; they will be interpreted in LilyPond's [\chordmode] *)
    }
    [@@deriving fields]

    type destructured = Core.Version.Content.destructured = {
      parts: part NEList.t;
      transitions: (Part_name.open_ * Part_name.open_ * part) list;
      default_structure: structure;
    }

    type t = Core.Version.Content.t =
      | Monolithic of {lilypond: string; bars: int; structure: structure} (** A tune as a full LilyPond, including clef, key, etc. *)
      | Destructured of destructured (** A tune decomposed as building blocks *)
    [@@deriving variants]
  end

  type source = Core.Version.source = {
    source: Core.Source.t Entry.t;
    structure: Content.structure;
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
