module Any = Any
module Book = Book
module Dance = Dance
module Person = Person
module Set = Set
module Source = Source
module Tune = Tune
module User = User
module Version = Version

type t = Connection.t

(* let open_ = Connection.open_ *)
(* let close = Connection.close *)
(* let with_ = Connection.with_ *)
let apply_migrations = Migrations.apply_migrations
