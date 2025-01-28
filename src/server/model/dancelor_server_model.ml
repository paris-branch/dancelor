(** {1 Server-side models} *)

open Dancelor_common

module Person : ModelBuilder.Person.S = Person
module Dance : ModelBuilder.Dance.S = Dance
module Tune : ModelBuilder.Tune.S = Tune
module Version : ModelBuilder.Version.S = Version
module VersionParameters = VersionParameters
module SetOrder = SetOrder
module Set : ModelBuilder.Set.S = Set
module SetParameters = SetParameters
module Book : ModelBuilder.Book.S = Book
module BookParameters = BookParameters
module Any : ModelBuilder.Any.S = Any
