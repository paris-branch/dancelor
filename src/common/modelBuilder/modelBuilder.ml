(** {1 Model builders}

    This module contains everything in the model that is common to both client
    and server implementation. *)

module Core = Core
module type Getters = Getters.S
module type S = Signature.S

module Build (Getters : Getters) : S = struct
  module Person = struct
    include Core.Person
    let get = Getters.get_person
  end
  module Source = Builder.Source.Build(Getters)
  module User = Builder.User.Build(Getters)
  module Dance = Builder.Dance.Build(Getters)
  module Tune = Builder.Tune.Build(Getters)
  module Version = Builder.Version.Build(Getters)
  module VersionParameters = Builder.VersionParameters.Build(Getters)
  module Set = Builder.Set.Build(Getters)
  module SetOrder = Core.SetOrder
  module SetParameters = Builder.SetParameters.Build(Getters)
  module Book = Builder.Book.Build(Getters)
  module BookParameters = Core.BookParameters
  module Any = Builder.Any.Build(Getters)
end
