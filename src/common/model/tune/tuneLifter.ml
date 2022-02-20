open Nes

module Lift
    (Credit : module type of CreditSignature)
    (Dance  : module type of  DanceSignature)
= struct
  include TuneCore

  let author tune =
    let%olwt author_slug = author tune in
    let%lwt author = Credit.get author_slug in
    Lwt.return_some author

  let dances tune =
    let%lwt dance_slugs = dances tune in
    Lwt_list.map_p Dance.get dance_slugs
end
