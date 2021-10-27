include TuneCore

module E = Dancelor_common_model.Tune_endpoints
module A = E.Arguments

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter;
  )
