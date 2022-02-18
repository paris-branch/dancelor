open Nes

module Lift
    (Credit : module type of CreditSignature)
= struct
  include DanceCore

  let deviser dance =
    let%olwt deviser_slug = deviser dance in
    let%lwt deviser = Credit.get deviser_slug in
    Lwt.return_some deviser
end
