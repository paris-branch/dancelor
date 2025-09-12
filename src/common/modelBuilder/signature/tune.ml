module type S = sig
  (** {1 Tune} *)

  open Nes

  type t = Core.Tune.t

  val make :
    names: NEString.t NEList.t ->
    kind: Kind.Base.t ->
    ?composers: Core.Person.t Entry.t list ->
    ?dances: Core.Dance.t Entry.t list ->
    ?remark: string ->
    ?scddb_id: int ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val names : t -> NEString.t NEList.t
  val names' : t Entry.t -> NEString.t NEList.t

  (** One name in the list of name. Picking it is deterministic. *)
  val one_name : t -> NEString.t
  val one_name' : t Entry.t -> NEString.t

  (** {!names} minus {!one_name}. *)
  val other_names : t -> NEString.t list
  val other_names' : t Entry.t -> NEString.t list

  val kind : t -> Kind.Base.t
  val kind' : t Entry.t -> Kind.Base.t

  val composers : t -> Core.Person.t Entry.t list Lwt.t
  val composers' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val dances : t -> Core.Dance.t Entry.t list Lwt.t
  val dances' : t Entry.t -> Core.Dance.t Entry.t list Lwt.t

  val remark : t -> string
  val remark' : t Entry.t -> string

  val scddb_id : t -> int option
  val scddb_id' : t Entry.t -> int option

  val date : t -> PartialDate.t option
  val date' : t Entry.t -> PartialDate.t option

  val slug : t -> Entry.Slug.t
  val slug' : t Entry.t -> Entry.Slug.t

  val equal : t -> t -> bool

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t option Lwt.t
end
