open Nes
open Dancelor_common_model
include TuneCore

let author = author >=>?| (Credit.get >=>| Lwt.return_some)
let dances = dances >=>| Lwt_list.map_p Dance.get

module E = TuneEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
