open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include BookLifted

module E = Common.Model.BookEndpoints
module A = E.Arguments

let make_and_save
    ?status ~title ?date ?contents ~modified_at ~created_at ()
  =
  Database.Book.save ~slug_hint:title @@ fun slug ->
  make ?status ~slug ~title ?date ?contents ~modified_at ~created_at ()

let () =
  Madge_server.(
    register ~endpoint:E.make_and_save @@ fun {a} {o} ->
    let%lwt contents =
      let%olwt contents = Lwt.return (o A.contents) in
      let%lwt contents = Lwt_list.map_s page_core_to_page contents in
      Lwt.return_some contents
    in
    make_and_save
      ?status:    (o A.status)
      ~title:     (a A.title)
      ?date:      (o A.date)
      ?contents
      ~modified_at: (a A.modified_at)
      ~created_at: (a A.created_at)
      ()
  )

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Book.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers: Lwt_list.[
        decreasing (Lwt.return %     date) (NesOption.compare NesPartialDate.compare) ;
        increasing (Lwt.return %    title) String.Sensible.compare ;
        increasing (Lwt.return %    title) String.compare_lengths ;
        increasing (Lwt.return % subtitle) String.Sensible.compare ;
        increasing (Lwt.return % subtitle) String.compare_lengths ;
      ]

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let search' ?pagination ?threshold filter =
  Lwt.map snd @@ search ?pagination ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let update
    ?status ~slug ~title ?date ?contents ~modified_at ~created_at
    ()
  =
  let%lwt book = make ?status ~slug ~title ?date ?contents ~modified_at ~created_at () in
  Database.Book.update book

let () =
  Madge_server.(
    register ~endpoint:E.update @@ fun {a} {o} ->
    let%lwt contents =
      let%olwt contents = Lwt.return (o A.contents) in
      let%lwt contents = Lwt_list.map_s page_core_to_page contents in
      Lwt.return_some contents
    in
    update
      ?status:    (o A.status)
      ~slug:      (a A.slug)
      ~title:     (a A.title)
      ?date:      (o A.date)
      ?contents
      ~modified_at: (a A.modified_at)
      ~created_at: (a A.created_at)
      ()
  )
