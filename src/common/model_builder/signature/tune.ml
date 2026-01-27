module type S = sig
  (** {1 Tune} *)

  open Nes

  type composer = Core.Tune.composer = {
    composer: Core.Person.entry;
    details: string;
  }

  val composer_composer : composer -> Core.Person.entry
  val composer_details : composer -> string

  type t = Core.Tune.t

  type access = Entry.Access.public [@@deriving yojson]
  type entry = t Entry.public

  val make :
    names: NEString.t NEList.t ->
    kind: Kind.Base.t ->
    ?composers: composer list ->
    ?dances: Core.Dance.entry list ->
    ?remark: string ->
    ?scddb_id: int ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val names : t -> NEString.t NEList.t
  val names' : entry -> NEString.t NEList.t

  (** One name in the list of name. Picking it is deterministic. *)
  val one_name : t -> NEString.t
  val one_name' : entry -> NEString.t

  (** {!names} minus {!one_name}. *)
  val other_names : t -> NEString.t list
  val other_names' : entry -> NEString.t list

  val kind : t -> Kind.Base.t
  val kind' : entry -> Kind.Base.t

  val composers : t -> composer list Lwt.t
  val composers' : entry -> composer list Lwt.t

  val dances : t -> Core.Dance.entry list Lwt.t
  val dances' : entry -> Core.Dance.entry list Lwt.t

  val remark : t -> string
  val remark' : entry -> string

  val scddb_id : t -> int option
  val scddb_id' : entry -> int option

  val date : t -> PartialDate.t option
  val date' : entry -> PartialDate.t option

  val slug : t -> NesSlug.t
  val slug' : entry -> NesSlug.t

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.id -> entry option Lwt.t
end
