include AnyLifted

module E = Dancelor_common_model.AnyEndpoints
module A = E.Arguments

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold  threshold;
    a A.filter     filter
  )

let search' ?pagination ?threshold filter =
  Lwt.map snd @@ search ?pagination ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let search_context ?threshold filter element =
  Madge_client.(
    call ~endpoint:E.search_context @@ fun {a} {o} ->
    o A.threshold threshold;
    a A.filter    filter;
    a A.element   element;
  )
