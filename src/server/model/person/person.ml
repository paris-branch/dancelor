open Nes
include PersonLifted

module E = Dancelor_common_model.PersonEndpoints
module A = E.Arguments

let make_and_save ?status ~name ~modified_at ~created_at () =
  Dancelor_server_database.Person.save ~slug_hint: name @@
  fun slug ->
  Lwt.return (make ?status ~slug ~name ~modified_at ~created_at ()) (* status should probably go in save *)

let () =
  Madge_server.(register ~endpoint: E.make_and_save @@
                fun { a } _ ->
                make_and_save
                  ~name: (a A.name)
                  ~modified_at: (a A.modified_at)
                  ~created_at: (a A.created_at)
                  ())

let search ?pagination ?(threshold = Float.min_float) filter =
  Dancelor_server_database.Person.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing
                [
                  increasing name String.Sensible.compare;
                  increasing name String.compare_lengths;
                ])
  >>=| Option.unwrap_map_or ~default: Lwt.return Pagination.apply pagination

let () =
  Madge_server.(register ~endpoint: E.search @@
                fun { a } { o } ->
                search
                  ?pagination: (o A.pagination)
                  ?threshold: (o A.threshold)
                  (a A.filter))
