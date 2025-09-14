open Nes
open Common
include ModelBuilder.Build(struct
  let get_book = lwt % Database.Book.get
  let get_dance = lwt % Database.Dance.get
  let get_person = lwt % Database.Person.get
  let get_set = lwt % Database.Set.get
  let get_source = lwt % Database.Source.get
  let get_tune = lwt % Database.Tune.get
  let get_version = lwt % Database.Version.get
end)
