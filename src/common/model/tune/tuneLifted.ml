open Nes

module Make
    (Credit : module type of CreditSignature)
    (Dance  : module type of  DanceSignature)
= struct
  include TuneCore

  let author = author >=>?| (Credit.get >=>| Lwt.return_some)
  let dances = dances >=>| Lwt_list.map_p Dance.get
end
