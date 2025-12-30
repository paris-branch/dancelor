module type S = sig
  (** {1 Version} *)

  open Nes

  module Part_name = Core.Version.Part_name
  module Structure = Core.Version.Structure
  module Voices = Core.Version.Voices
  module Content = Core.Version.Content

  type source = Core.Version.source = {
    source: Core.Source.entry;
    structure: Structure.t;
    details: string;
  }

  val source_source : source -> Core.Source.entry
  val source_structure : source -> Structure.t
  val source_details : source -> string

  type t = Core.Version.t

  type access = Entry.Access.public [@@deriving yojson]
  type entry = t Entry.public

  val make :
    tune: Core.Tune.entry ->
    key: Music.Key.t ->
    ?sources: source list ->
    ?arrangers: Core.Person.entry list ->
    ?remark: string ->
    ?disambiguation: string ->
    content: Content.t ->
    unit ->
    t

  val tune : t -> Core.Tune.entry Lwt.t
  val tune' : entry -> Core.Tune.entry Lwt.t

  val key : t -> Music.Key.t
  val key' : entry -> Music.Key.t

  val sources : t -> source list Lwt.t
  val sources' : entry -> source list Lwt.t

  val sources_grouped : t -> source list list Lwt.t
  val sources_grouped' : entry -> source list list Lwt.t

  val arrangers : t -> Core.Person.entry list Lwt.t
  val arrangers' : entry -> Core.Person.entry list Lwt.t

  val remark : t -> string
  val remark' : entry -> string

  val disambiguation : t -> string
  val disambiguation' : entry -> string

  val content : t -> Content.t
  val content' : entry -> Content.t
  (** Raises {!Failure} on the client side. *)

  val content_lilypond : ?structure: Structure.t -> ?content: Content.t -> t -> string Lwt.t
  val content_lilypond' : ?structure: Structure.t -> ?content: Content.t -> entry -> string Lwt.t
  (** Convenient wrapper around {!Content.lilypond} that grabs the right
      information from {!tune}. If the optional [?content] argument is not
      provided, the content is taken from the version with {!content} which
      raise an exception on the client side. *)

  val names : t -> NEString.t NEList.t Lwt.t
  val names' : entry -> NEString.t NEList.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.names}. *)

  val one_name : t -> NEString.t Lwt.t
  val one_name' : entry -> NEString.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.one_name}. *)

  val other_names : t -> NEString.t list Lwt.t
  val other_names' : entry -> NEString.t list Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.other_names}. *)

  val kind : t -> Kind.Base.t Lwt.t
  val kind' : entry -> Kind.Base.t Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.kind}. *)

  val slug : t -> NesSlug.t Lwt.t
  val slug' : entry -> NesSlug.t Lwt.t

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  val set_content : Content.t -> t -> t

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> entry option Lwt.t
end
