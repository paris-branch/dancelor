include SetLifted

module E = Dancelor_common_model.SetEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?devisers ~kind ?versions_and_parameters
    ~order ?dances ~modified_at ~created_at
    ()
  =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name;
    o A.devisers devisers;
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

let search ?slice ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.slice slice;
    o A.threshold threshold;
    a A.filter filter;
  )

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
