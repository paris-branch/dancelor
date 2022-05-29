include BookLifted

module E = Dancelor_common_model.BookEndpoints
module A = E.Arguments

let make_and_save
    ?status ~title ?date ?contents_and_parameters ()
  =
  let%lwt contents_and_parameters =
    let%olwt contents = Lwt.return contents_and_parameters in
    let%lwt contents = Lwt_list.map_s page_to_page_core contents in
    Lwt.return_some contents
  in
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.title title;
    o A.date date;
    o A.contents_and_parameters contents_and_parameters
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter;
  )

let update
    ?status ~slug ~title ?date ?contents_and_parameters ()
  =
  Madge_client.(
    call ~endpoint:E.update @@ fun {a} {o} ->
    o A.status status;
    a A.slug slug;
    a A.title title;
    o A.date date;
    o A.contents_and_parameters contents_and_parameters
  )
