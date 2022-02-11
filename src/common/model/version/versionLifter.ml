open Nes

module Lift
    (Credit : module type of CreditSignature)
    (Tune   : module type of   TuneSignature)
= struct
  include VersionCore

  let tune version =
    let%lwt tune_slug = tune version in
    Tune.get tune_slug

  let arranger tune =
    let%optlwt arranger_slug = arranger tune in
    let%lwt arranger = Credit.get arranger_slug in
    Lwt.return_some arranger
end
