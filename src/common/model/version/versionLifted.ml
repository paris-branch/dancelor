open Nes

module Make
    (Credit : module type of CreditSignature)
    (Tune   : module type of   TuneSignature)
= struct
  include VersionCore

  let tune = tune >=>| Tune.get
  let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
end
