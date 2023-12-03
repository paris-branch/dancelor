open Nes
module Common = Dancelor_common

include BookLifted

module E = Common.Model.BookEndpoints
module A = E.Arguments

let make_and_save
    ?status ~title ?date ?contents ~modified_at ~created_at ()
  =
  Dancelor_server_database.Book.save ~slug_hint:title @@ fun slug ->
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

let search ?pagination ?(threshold=Float.min_float) filter =
  let module Score = Common.Model.Score in
  let%lwt results =
    Dancelor_server_database.Book.get_all ()
    >>=| Score.lwt_map_from_list (Filter.accepts filter)
    >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
    >>=| Score.(list_proj_sort_decreasing [
        decreasing (Lwt.return %     date) (NesOption.compare NesPartialDate.compare) ;
        increasing (Lwt.return %    title) String.Sensible.compare ;
        increasing (Lwt.return %    title) String.compare_lengths ;
        increasing (Lwt.return % subtitle) String.Sensible.compare ;
        increasing (Lwt.return % subtitle) String.compare_lengths ;
      ])
  in
  Lwt.return @@ Option.fold ~none:Fun.id ~some:Common.Model.Pagination.apply pagination results
(* FIXME: Simplify [list_proj_sort_decreasing] and [decreasing] and
   [increasing] because they probably don't need Lwt anymore. *)

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let update
    ?status ~slug ~title ?date ?contents ~modified_at ~created_at
    ()
  =
  let%lwt book = make ?status ~slug ~title ?date ?contents ~modified_at ~created_at () in
  Dancelor_server_database.Book.update book

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
