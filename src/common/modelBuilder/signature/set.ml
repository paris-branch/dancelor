module type S = sig
  (** {1 Set} *)

  open Nes

  type t = Core.Set.t

  val make :
    name: string ->
    ?conceptors: Core.Person.t Entry.t list ->
    kind: Kind.Dance.t ->
    ?contents: (Core.Version.t Entry.t * Core.VersionParameters.t) list ->
    order: Core.SetOrder.t ->
    ?dances: Core.Dance.t Entry.t list ->
    unit ->
    t

  val name : t -> string
  val name' : t Entry.t -> string

  val conceptors : t -> Core.Person.t Entry.t list Lwt.t
  val conceptors' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val kind : t -> Kind.Dance.t
  val kind' : t Entry.t -> Kind.Dance.t

  val contents : t -> (Core.Version.t Entry.t * Core.VersionParameters.t) list Lwt.t
  val contents' : t Entry.t -> (Core.Version.t Entry.t * Core.VersionParameters.t) list Lwt.t

  val order : t -> Core.SetOrder.t
  val order' : t Entry.t -> Core.SetOrder.t

  val instructions : t -> string
  val instructions' : t Entry.t -> string

  val dances : t -> Core.Dance.t Entry.t list Lwt.t
  val dances' : t Entry.t -> Core.Dance.t Entry.t list Lwt.t

  val remark : t -> string
  val remark' : t Entry.t -> string

  val contains_version : Core.Version.t Entry.Id.t -> t Entry.t -> bool
  (** REVIEW: This really takes a id? *)

  val find_context : int -> t -> Core.Version.t Entry.t List.context option Lwt.t
  val find_context' : int -> t Entry.t -> Core.Version.t Entry.t List.context option Lwt.t
  (** Given an indice and a set, find the context around that indice in the
      set. *)

  val compare : t Entry.t -> t Entry.t -> int
  val equal : t Entry.t -> t Entry.t -> bool

  val lilypond_content_cache_key : t -> string Lwt.t
  val lilypond_content_cache_key' : t Entry.t -> string Lwt.t

  (* {2 Warnings} *)

  type warning = Core.Set.warning =
    | Empty
    | WrongKind
    | WrongVersionBars of Core.Version.t Entry.t
    | WrongVersionKind of Core.Tune.t Entry.t
    | DuplicateVersion of Core.Tune.t Entry.t

  type warnings = warning list

  val warnings : t -> warnings Lwt.t
  val warnings' : t Entry.t -> warnings Lwt.t

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t Lwt.t
end
