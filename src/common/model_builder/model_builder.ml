(** {1 Model builders}

    This module contains everything in the model that is common to both client
    and server implementation. *)

module Core = Core
module Signature = Signature

module type Getters = Getters.S

module type S = sig
  module User : Signature.User
  module Person : Signature.Person
  module Source : Signature.Source
  module Dance : Signature.Dance
  module Tune : Signature.Tune
  module Version : Signature.Version
  module Version_parameters : Signature.Version_parameters
  module Set : Signature.Set
  module Set_order : Signature.Set_order
  module Set_parameters : Signature.Set_parameters
  module Book : Signature.Book
  module Book_parameters : Signature.Book_parameters
  module Any : Signature.Any
end

module Build (Getters : Getters) : S = struct
  module User = struct
    include Core.User
    let get = Getters.get_user
  end
  module Person = struct
    include Core.Person
    let get = Getters.get_person
  end
  module Source = struct
    include Core.Source
    let get = Getters.get_source
  end
  module Dance = struct
    include Core.Dance
    let get = Getters.get_dance
  end
  module Tune = struct
    include Core.Tune
    let get = Getters.get_tune
  end
  module Version = Builder.Version.Build(Getters)
  module Version_parameters = Core.Version_parameters
  module Set = Builder.Set.Build(Getters)
  module Set_order = Core.Set_order
  module Set_parameters = Core.Set_parameters
  module Book = Builder.Book.Build(Getters)
  module Book_parameters = Core.Book_parameters
  module Any = Builder.Any.Build(Getters)
end
