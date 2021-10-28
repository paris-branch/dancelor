open Nes
include AnyCore

module E = Dancelor_common_model.Any_endpoints
module A = E.Arguments

let search ?pagination ?(threshold=Float.min_float) filter =
  let%lwt credits  = Dancelor_server_database.Credit.get_all ()  >|=| List.map (fun c -> Credit c) in
  let%lwt dances   = Dancelor_server_database.Dance.get_all ()   >|=| List.map (fun d -> Dance d) in
  let%lwt persons  = Dancelor_server_database.Person.get_all ()  >|=| List.map (fun p -> Person p) in
  let%lwt books    = Dancelor_server_database.Book.get_all ()    >|=| List.map (fun b -> Book b) in
  let%lwt sets     = Dancelor_server_database.Set.get_all ()     >|=| List.map (fun s -> Set s) in
  (* let%lwt sources  = Dancelor_server_database.Source.get_all ()  >|=| List.map (fun s -> Source s) in *)
  let%lwt tunes    = Dancelor_server_database.Tune.get_all ()    >|=| List.map (fun t -> Tune t) in
  let%lwt versions = Dancelor_server_database.Version.get_all () >|=| List.map (fun v -> Version v) in
  (Lwt.return (credits @ dances @ persons @ books @ sets @ tunes @ versions))
  >>=| Score.lwt_map_from_list (AnyFilter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold:(o A.threshold)
      (a A.filter)
  )
