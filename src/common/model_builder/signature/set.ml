module type S = sig
  (** {1 Set} *)

  open Nes

  type t = Core.Set.t

  type access = Entry.Access.Private.t [@@deriving yojson]
  type entry = t Entry.private_

  val make :
    name: NEString.t ->
    conceptors: Core.Person.t Entry.id list ->
    kind: Kind.Dance.t ->
    contents: (Core.Version.t Entry.id * Core.Version_parameters.t) list ->
    order: Core.Set_order.t ->
    remark: NEString.t option ->
    unit ->
    t

  val name : t -> NEString.t
  val name' : entry -> NEString.t

  val conceptors : t -> Core.Person.t Entry.id list
  val conceptors' : entry -> Core.Person.t Entry.id list

  val kind : t -> Kind.Dance.t
  val kind' : entry -> Kind.Dance.t

  val contents : t -> (Core.Version.t Entry.id * Core.Version_parameters.t) list
  val contents' : entry -> (Core.Version.t Entry.id * Core.Version_parameters.t) list

  val order : t -> Core.Set_order.t
  val order' : entry -> Core.Set_order.t

  val remark : t -> NEString.t option
  val remark' : entry -> NEString.t option

  val slug : t -> NesSlug.t
  val slug' : entry -> NesSlug.t

  val find_context : int -> t -> Core.Version.t Entry.id List.context option
  val find_context' : int -> entry -> Core.Version.t Entry.id List.context option
  (** Given an indice and a set, find the context around that indice in the
      set. *)

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  val set_contents : (Core.Version.t Entry.id * Core.Version_parameters.t) list -> t -> t

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
