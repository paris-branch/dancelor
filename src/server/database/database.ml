module Any = Any
module Book = Book
module Dance = Dance
module Person = Person
module Set = Set
module Source = Source
module Tune = Tune
module User = User
module Version = Version
module Utils = Utils

type t = Connection.t

module Migrations = Migrations
let apply_migrations = Migrations.apply_migrations
