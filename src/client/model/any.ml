open Dancelor_common_model
module E = Any_endpoints
module A = E.Arguments

include Any

let search ?pagination ?threshold ?except string =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    o A.type_ except;
    a A.string string
  )
