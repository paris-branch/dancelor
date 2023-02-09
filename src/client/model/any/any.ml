include AnyLifted

module E = Dancelor_common_model.AnyEndpoints
module A = E.Arguments

let search ?pagination ?threshold filter =
  Madge_client.(call ~endpoint: E.search
  @@ fun { a } { o } ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter)

let count ?threshold filter =
  Madge_client.(call ~endpoint: E.count
  @@ fun { a } { o } ->
    o A.threshold threshold;
    a A.filter filter)
