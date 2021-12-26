open Nes
open Dancelor_common_model
include VersionCore

let tune = tune >=>| Tune.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)

let content _t =
  assert false (* FIXME *)

module E = VersionEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
