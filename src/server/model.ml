open Nes
open Dancelor_common
include Model_builder.Build(struct
  let get_user = lwt % Database.User.get
  let get_book = Database.Book.get
  let get_dance = Database.Dance.get
  let get_person = Database.Person.get
  let get_set = Database.Set.get
  let get_source = Database.Source.get
  let get_tune = Database.Tune.get
  let get_version = lwt % Database.Version.get
end)
