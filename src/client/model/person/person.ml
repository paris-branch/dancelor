include PersonLifted

module E = Dancelor_common_model.PersonEndpoints
module A = E.Arguments

let make_and_save ?status ~name ~modified_at ~created_at () =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name;
    a A.modified_at modified_at;
    a A.created_at created_at;
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter
  )
