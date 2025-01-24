(** {1 Server-side models} *)

include Dancelor_common_model_utils

(** {2 Modules overriden on the client side} *)

module Person : Dancelor_common_model.Person.Signature = Person
module Dance : Dancelor_common_model.Dance.Signature = Dance
module Tune : Dancelor_common_model.Tune.Signature = Tune
module Version : Dancelor_common_model.Version.Signature = Version
module VersionParameters = VersionParameters
module SetOrder = SetOrder
module Set : Dancelor_common_model.Set.Signature = Set
module SetParameters = SetParameters
module Book : Dancelor_common_model.Book.Signature = Book
module BookParameters = BookParameters
module Any : Dancelor_common_model.Any.Signature = Any
