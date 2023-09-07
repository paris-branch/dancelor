open Nes
include BookLifted

module E = Dancelor_common_model.BookEndpoints
module A = E.Arguments

let make_and_save
    ?status
    ~title
    ?date
    ?contents_and_parameters
    ~modified_at
    ~created_at
    ()
  =
  Dancelor_server_database.Book.save ~slug_hint: title @@
  fun slug ->
  make ?status ~slug ~title ~date ?contents_and_parameters ~modified_at ~created_at ()

let () =
  Madge_server.(register ~endpoint: E.make_and_save @@
                fun { a } { o } ->
                let%lwt contents_and_parameters =
                  let%olwt contents = Lwt.return (o A.contents_and_parameters) in
                  let%lwt contents = Lwt_list.map_s page_core_to_page contents in
                  Lwt.return_some contents
                in
                make_and_save
                  ?status: (o A.status)
                  ~title: (a A.title)
                  ?date: (o A.date)
                  ?contents_and_parameters
                  ~modified_at: (a A.modified_at)
                  ~created_at: (a A.created_at)
                  ())

let search ?pagination ?(threshold = Float.min_float) filter =
  Dancelor_server_database.Book.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing
                [
                  decreasing date (NesOption.compare NesPartialDate.compare);
                  increasing title String.Sensible.compare;
                  increasing title String.compare_lengths;
                  increasing subtitle String.Sensible.compare;
                  increasing subtitle String.compare_lengths;
                ])
  >>=| Option.unwrap_map_or ~default: Lwt.return Pagination.apply pagination

let () =
  Madge_server.(register ~endpoint: E.search @@
                fun { a } { o } ->
                search
                  ?pagination: (o A.pagination)
                  ?threshold: (o A.threshold)
                  (a A.filter))

let update
    ?status
    ~slug
    ~title
    ?date
    ?contents_and_parameters
    ~modified_at
    ~created_at
    ()
  =
  let%lwt book = make ?status ~slug ~title ~date ?contents_and_parameters ~modified_at ~created_at () in
  Dancelor_server_database.Book.update book

let () =
  Madge_server.(register ~endpoint: E.update @@
                fun { a } { o } ->
                let%lwt contents_and_parameters =
                  let%olwt contents = Lwt.return (o A.contents_and_parameters) in
                  let%lwt contents = Lwt_list.map_s page_core_to_page contents in
                  Lwt.return_some contents
                in
                update
                  ?status: (o A.status)
                  ~slug: (a A.slug)
                  ~title: (a A.title)
                  ?date: (o A.date)
                  ?contents_and_parameters
                  ~modified_at: (a A.modified_at)
                  ~created_at: (a A.created_at)
                  ())
