open Common
include ModelBuilder.Build(struct
  let get_book = Database.Book.get
  let get_dance = Database.Dance.get
  let get_person = Database.Person.get
  let get_set = Database.Set.get
  let get_source = Database.Source.get
  let get_tune = Database.Tune.get
  let get_version = Database.Version.get
end)
