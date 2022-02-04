open Nes

module Lift
    (Credit : module type of CreditSignature)
= struct
  include DanceCore

  let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)
end
