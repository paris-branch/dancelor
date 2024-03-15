include TuneLifted

module E = Dancelor_common_model.TuneEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ?alternative_names ~kind ?composers ?dances
    ?remark ?scddb_id ?date ~modified_at ~created_at ()
  =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name;
    o A.alternative_names alternative_names;
    a A.kind kind;
    o A.composers composers;
    o A.dances dances;
    o A.remark remark;
    o A.scddb_id scddb_id;
    o A.date date;
    a A.modified_at modified_at;
    a A.created_at created_at;
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
