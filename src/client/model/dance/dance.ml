include DanceLifted

module E = Dancelor_common_model.DanceEndpoints
module A = E.Arguments

let make_and_save
    ?status
    ~name
    ~kind
    ?devisers
    ?two_chords
    ?scddb_id
    ?disambiguation
    ?date
    ~modified_at
    ~created_at
    ()
  =
  Madge_client.(
    call ~endpoint: E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name;
    a A.kind kind;
    o A.devisers devisers;
    o A.two_chords two_chords;
    o A.scddb_id scddb_id;
    o A.disambiguation disambiguation;
    o A.date date;
    a A.modified_at modified_at;
    a A.created_at created_at;
  )

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
