(** {1 Filter builders} *)

module Core = Core
module type S = Signature.S

module Build (Model : ModelBuilder.S) : S = struct
  module Accepts = Accepts.Make(Model)

  module Source = struct
    include Core.Source
    let accepts = Accepts.accepts_source
  end

  module Person = struct
    include Core.Person
    let accepts = Accepts.accepts_person
  end

  module Dance = struct
    include Core.Dance
    let accepts = Accepts.accepts_dance
  end

  module Tune = struct
    include Core.Tune
    let accepts = Accepts.accepts_tune
  end

  module Version = struct
    include Core.Version
    let accepts = Accepts.accepts_version
  end

  module Set = struct
    include Core.Set
    let accepts = Accepts.accepts_set
  end

  module Book = struct
    include Core.Book
    let accepts = Accepts.accepts_book
  end

  module Any = struct
    include Core.Any
    let accepts = Accepts.accepts_any
  end
end
