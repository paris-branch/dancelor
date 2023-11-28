include SetLifted

module E = Dancelor_common_model.SetEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?deviser ~kind ?versions_and_parameters
    ~order ?dances ~modified_at ~created_at
    ()
  =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name;
    o A.deviser deviser;
    a A.kind kind;
    o A.versions_and_parameters versions_and_parameters;
    a A.order order;
    o A.dances dances;
    a A.modified_at modified_at;
    a A.created_at created_at;
  )

let delete s =
  Madge_client.(
    call ~endpoint:E.delete @@ fun {a} _ ->
    a A.slug (slug s)
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter;
  )

let count filter =
  Madge_client.(
    call ~endpoint:E.count @@ fun {a} _ ->
    a A.filter filter;
  )
