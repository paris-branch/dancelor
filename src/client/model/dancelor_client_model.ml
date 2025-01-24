(** {1 Client-side models} *)

module Common = Dancelor_common
include Dancelor_common_model_utils

(** {2 Modules overriden on the client side} *)

module Person : Common.Model.Person.S = Person
module Dance : Common.Model.Dance.S = Dance
module Tune : Common.Model.Tune.S = Tune
module Version : Common.Model.Version.S = Version
module VersionParameters = VersionParameters
module SetOrder = SetOrder
module Set : Common.Model.Set.S = Set
module SetParameters = SetParameters
module Book : Common.Model.Book.S = Book
module BookParameters = BookParameters
module Any : Common.Model.Any.S = Any
