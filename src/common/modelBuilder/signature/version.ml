module type S = sig
  (** {1 Version} *)

  open Nes

  module Part_name = Core.Version.Part_name
  module Structure = Core.Version.Structure
  module Voices = Core.Version.Voices
  module Content = Core.Version.Content

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
