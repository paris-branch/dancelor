module type S = sig
  module User : User.S
  module Source : Source.S
  module Person : Person.S
  module Dance : Dance.S
  module Tune : Tune.S
  module Version : Version.S
  module Set : Set.S
  module Book : Book.S
  module Any : Any.S
end
