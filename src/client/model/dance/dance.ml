include DanceLifted

module E = Dancelor_common_model.DanceEndpoints
module A = E.Arguments

let make_and_save
    ?status ~name ~kind ?deviser ~two_chords ?scddb_id ()
  =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name;
    a A.kind kind;
    o A.deviser deviser;
    a A.two_chords two_chords;
    o A.scddb_id scddb_id
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter
  )
