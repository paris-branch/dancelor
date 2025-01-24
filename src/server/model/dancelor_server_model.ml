(** {1 Server-side models} *)

module Core = Dancelor_common_model_core
module Signature = Dancelor_common_model_signature

include Dancelor_common_model_utils

(** {2 Modules overriden on the client side} *)

module Person : Signature.Person = Person
module Dance : Signature.Dance = Dance
module Tune : Signature.Tune = Tune

module Version : Signature.Version = Version
module VersionParameters = VersionParameters

module SetOrder = SetOrder
module Set : Signature.Set = Set
module SetParameters = SetParameters

module Book : Signature.Book = Book
module BookParameters = BookParameters

module Any : Signature.Any = Any
