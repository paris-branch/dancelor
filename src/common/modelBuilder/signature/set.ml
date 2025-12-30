module type S = sig
  (** {1 Set} *)

  open Nes

  type t = Core.Set.t

  type access = Entry.Access.private_ [@@deriving yojson]
  type entry = t Entry.private_

  val make :
    name: NEString.t ->
    ?conceptors: Core.Person.entry list ->
    kind: Kind.Dance.t ->
    ?contents: (Core.Version.entry * Core.VersionParameters.t) list ->
    order: Core.SetOrder.t ->
    ?dances: Core.Dance.entry list ->
    unit ->
    t

  val name : t -> NEString.t
  val name' : entry -> NEString.t

  val conceptors : t -> Core.Person.entry list Lwt.t
  val conceptors' : entry -> Core.Person.entry list Lwt.t

  val kind : t -> Kind.Dance.t
  val kind' : entry -> Kind.Dance.t

  val contents : t -> (Core.Version.entry * Core.VersionParameters.t) list Lwt.t
  val contents' : entry -> (Core.Version.entry * Core.VersionParameters.t) list Lwt.t

  val order : t -> Core.SetOrder.t
  val order' : entry -> Core.SetOrder.t

  val instructions : t -> string
  val instructions' : entry -> string

  val dances : t -> Core.Dance.entry list Lwt.t
  val dances' : entry -> Core.Dance.entry list Lwt.t

  val remark : t -> string
  val remark' : entry -> string

  val slug : t -> NesSlug.t
  val slug' : entry -> NesSlug.t

  val find_context : int -> t -> Core.Version.entry List.context option Lwt.t
  val find_context' : int -> entry -> Core.Version.entry List.context option Lwt.t
  (** Given an indice and a set, find the context around that indice in the
      set. *)

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  val set_contents : (Core.Version.entry * Core.VersionParameters.t) list -> t -> t

  (* {2 Warnings} *)

  type warning = Core.Set.warning =
    | Empty
    | Duplicate_tune of Core.Tune.entry

  type warnings = warning list

  val warnings : t -> warnings Lwt.t
  val warnings' : entry -> warnings Lwt.t

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> entry option Lwt.t
end
