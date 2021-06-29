open Nes
module E = Dancelor_common_model.Dance_endpoints
module A = E.Arguments

include Dancelor_common_model.Dance

let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.string string
  )
