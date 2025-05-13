module type S = sig
  (** {1 Version} *)

  open Nes

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
    content: string ->
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

  val content : t -> string
  val content' : t Entry.t -> string

  val kind : t -> Kind.Version.t Lwt.t
  val kind' : t Entry.t -> Kind.Version.t Lwt.t
  (** Convenient wrapper around {!bars} and {!Tune.kind}. *)

  val name : t -> string Lwt.t
  val name' : t Entry.t -> string Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.name}. *)

  val equal : t -> t -> bool

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Slug.t -> t Entry.t Lwt.t
end
