include VersionLifted

module E = Dancelor_common_model.VersionEndpoints
module A = E.Arguments

let make_and_save
    ?status
    ~tune
    ~bars
    ~key
    ~structure
    ?arrangers
    ?remark
    ?disambiguation
    ~content
    ~modified_at
    ~created_at
    ()
  =
  Madge_client.(
    call ~endpoint: E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.tune tune;
    a A.bars bars;
    a A.key key;
    a A.structure structure;
    o A.arrangers arrangers;
    o A.remark remark;
    o A.disambiguation disambiguation;
    a A.content content;
    a A.modified_at modified_at;
    a A.created_at created_at;
  )

let search ?slice ?threshold filter =
  Madge_client.(
    call ~endpoint: E.search @@ fun {a} {o} ->
    o A.slice slice;
    o A.threshold threshold;
    a A.filter filter;
  )

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
