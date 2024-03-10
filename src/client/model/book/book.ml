include BookLifted

module E = Dancelor_common_model.BookEndpoints
module A = E.Arguments

let make_and_save
    ?status ~title ?date ?contents ~modified_at ~created_at
    ()
  =
  let contents = Option.map (List.map page_to_page_core) contents in
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.title title;
    o A.date date;
    o A.contents contents;
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

let update
    ?status ~slug ~title ?date ?contents ~modified_at ~created_at
    ()
  =
  let contents = Option.map (List.map page_to_page_core) contents in
  Madge_client.(
    call ~endpoint:E.update @@ fun {a} {o} ->
    o A.status status;
    a A.slug slug;
    a A.title title;
    o A.date date;
    o A.contents contents;
    a A.modified_at modified_at;
    a A.created_at created_at;
  )
