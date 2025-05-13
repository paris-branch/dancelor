module type S = sig
  module Source : Source.S
  module Person : Person.S
  module User : User.S
  module Dance : Dance.S
  module Tune : Tune.S
  module Version : Version.S
  module VersionParameters : VersionParameters.S
  module Set : Set.S
  module SetOrder : SetOrder.S
  module SetParameters : SetParameters.S
  module Book : Book.S
  module BookParameters : BookParameters.S
  module Any : Any.S
end
