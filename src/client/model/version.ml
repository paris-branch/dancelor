include VersionCore

module E = Dancelor_common_model.VersionEndpoints
module A = E.Arguments

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter;
  )

let count ?threshold filter =
  Madge_client.(
    call ~endpoint:E.count @@ fun {a} {o} ->
    o A.threshold threshold;
    a A.filter filter
  )

let mark_fixed version =
  Madge_client.(
    call ~endpoint:E.mark_fixed @@ fun {a} _ ->
    a A.version version
  )

let mark_broken version =
  Madge_client.(
    call ~endpoint:E.mark_broken @@ fun {a} _ ->
    a A.version version
  )
