open Nes

module type S = sig
  val get_book : Core.Book.t Slug.t -> Core.Book.t Entry.t Lwt.t
  val get_dance : Core.Dance.t Slug.t -> Core.Dance.t Entry.t Lwt.t
  val get_person : Core.Person.t Slug.t -> Core.Person.t Entry.t Lwt.t
  val get_set : Core.Set.t Slug.t -> Core.Set.t Entry.t Lwt.t
  val get_source : Core.Source.t Slug.t -> Core.Source.t Entry.t Lwt.t
  val get_tune : Core.Tune.t Slug.t -> Core.Tune.t Entry.t Lwt.t
  val get_version : Core.Version.t Slug.t -> Core.Version.t Entry.t Lwt.t
end
