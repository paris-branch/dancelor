module type S = sig
  module Source : Source.S
  module Person : Person.S
  module User : User.S
  module Dance : Dance.S
  module Tune : Tune.S
  module Version : Version.S
  module Version_parameters : Version_parameters.S
  module Set : Set.S
  module Set_order : Set_order.S
  module Set_parameters : Set_parameters.S
  module Book : Book.S
  module Book_parameters : Book_parameters.S
  module Any : Any.S
end
