include CreditCore

module E = Dancelor_common_model.CreditEndpoints
module A = E.Arguments

let make_and_save ?status ~line ?persons ?scddb_id () =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.line line;
    o A.persons persons;
    o A.scddb_id scddb_id
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter
  )
