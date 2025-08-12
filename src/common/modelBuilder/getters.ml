open Nes

module type S = sig
  val get_book : Core.Book.t Entry.Id.t -> Core.Book.t Entry.t option Lwt.t
  val get_dance : Core.Dance.t Entry.Id.t -> Core.Dance.t Entry.t option Lwt.t
  val get_person : Core.Person.t Entry.Id.t -> Core.Person.t Entry.t option Lwt.t
  val get_set : Core.Set.t Entry.Id.t -> Core.Set.t Entry.t option Lwt.t
  val get_source : Core.Source.t Entry.Id.t -> Core.Source.t Entry.t option Lwt.t
  val get_tune : Core.Tune.t Entry.Id.t -> Core.Tune.t Entry.t option Lwt.t
  val get_version : Core.Version.t Entry.Id.t -> Core.Version.t Entry.t option Lwt.t
end
