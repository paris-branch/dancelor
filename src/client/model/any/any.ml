include AnyLifted

module E = Dancelor_common_model.AnyEndpoints
module A = E.Arguments

let search ?slice ?threshold filter =
  Madge_client.(
    call ~endpoint: E.search @@ fun {a} {o} ->
    o A.slice slice;
    o A.threshold threshold;
    a A.filter filter
  )

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let search_context ?threshold filter element =
  Madge_client.(
    call ~endpoint: E.search_context @@ fun {a} {o} ->
    o A.threshold threshold;
    a A.filter filter;
    a A.element element;
  )
