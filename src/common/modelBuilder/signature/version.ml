module type S = sig
  (** {1 Version} *)

  open Nes

  module Content : sig
    type t = Core.Version.Content.t =
      | Full of string
    (** A tune as a full LilyPond, including clef, key, etc. *)
    [@@deriving variants]
  end

  type t = Core.Version.t

  val make :
    tune: Core.Tune.t Entry.t ->
    bars: int ->
    key: Music.key ->
    structure: string ->
    ?sources: Core.Source.t Entry.t list ->
    ?arrangers: Core.Person.t Entry.t list ->
    ?remark: string ->
    ?disambiguation: string ->
    content: Content.t ->
    unit ->
    t

  val tune : t -> Core.Tune.t Entry.t Lwt.t
  val tune' : t Entry.t -> Core.Tune.t Entry.t Lwt.t

  val bars : t -> int
  val bars' : t Entry.t -> int

  val key : t -> Music.key
  val key' : t Entry.t -> Music.key

  val structure : t -> string
  val structure' : t Entry.t -> string

  val sources : t -> Core.Source.t Entry.t list Lwt.t
  val sources' : t Entry.t -> Core.Source.t Entry.t list Lwt.t

  val arrangers : t -> Core.Person.t Entry.t list Lwt.t
  val arrangers' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val remark : t -> string
  val remark' : t Entry.t -> string

  val disambiguation : t -> string
  val disambiguation' : t Entry.t -> string

  val content : t -> Content.t
  val content' : t Entry.t -> Content.t

  val kind : t -> Kind.Version.t Lwt.t
  val kind' : t Entry.t -> Kind.Version.t Lwt.t
  (** Convenient wrapper around {!bars} and {!Tune.kind}. *)

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

  val set_content : Content.t option -> t -> t

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t option Lwt.t
end
