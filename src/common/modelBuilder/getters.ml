open Nes

module type S = sig
  val get_user : Core.User.t Entry.Id.t -> Core.User.entry option Lwt.t
  val get_book : Core.Book.t Entry.Id.t -> Core.Book.entry option Lwt.t
  val get_dance : Core.Dance.t Entry.Id.t -> Core.Dance.entry option Lwt.t
  val get_person : Core.Person.t Entry.Id.t -> Core.Person.entry option Lwt.t
  val get_set : Core.Set.t Entry.Id.t -> Core.Set.entry option Lwt.t
  val get_source : Core.Source.t Entry.Id.t -> Core.Source.entry option Lwt.t
  val get_tune : Core.Tune.t Entry.Id.t -> Core.Tune.entry option Lwt.t
  val get_version : Core.Version.t Entry.Id.t -> Core.Version.entry option Lwt.t
end
