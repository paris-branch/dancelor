open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include AnyLifted

module E = Common.Model.AnyEndpoints
module A = E.Arguments

let search ?pagination ?(threshold=Float.min_float) filter =
  let module Score = Common.Model.Score in
  let%lwt persons  = Database.Person.get_all ()  >|=| List.map (fun c -> Person c) in
  let%lwt dances   = Database.Dance.get_all ()   >|=| List.map (fun d -> Dance d) in
  let%lwt books    = Database.Book.get_all ()    >|=| List.map (fun b -> Book b) in
  let%lwt sets     = Database.Set.get_all ()     >|=| List.map (fun s -> Set s) in
  let%lwt tunes    = Database.Tune.get_all ()    >|=| List.map (fun t -> Tune t) in
  let%lwt versions = Database.Version.get_all () >|=| List.map (fun v -> Version v) in
  let%lwt results =
    (Lwt.return (persons @ dances @ books @ sets @ tunes @ versions))
    >>=| Score.lwt_map_from_list (Filter.accepts filter)
    >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
    >>=| Score.(list_proj_sort_decreasing [])
  in
  Lwt.return @@ Option.fold ~none:Fun.id ~some:Common.Model.Pagination.apply pagination results

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold:(o A.threshold)
      (a A.filter)
  )

let count ?threshold filter =
  let%lwt l = search ?threshold filter in
  Lwt.return (List.length l)

let () =
  Madge_server.(
    register ~endpoint:E.count @@ fun {a} {o} ->
    count ?threshold:(o A.threshold) (a A.filter)
  )
