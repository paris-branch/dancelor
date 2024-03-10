open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include AnyLifted

module E = Common.Model.AnyEndpoints
module A = E.Arguments

let get_all () =
  let%lwt persons  = Database.Person.get_all ()  >|=| List.map (fun c -> Person c) in
  let%lwt dances   = Database.Dance.get_all ()   >|=| List.map (fun d -> Dance d) in
  let%lwt books    = Database.Book.get_all ()    >|=| List.map (fun b -> Book b) in
  let%lwt sets     = Database.Set.get_all ()     >|=| List.map (fun s -> Set s) in
  let%lwt tunes    = Database.Tune.get_all ()    >|=| List.map (fun t -> Tune t) in
  let%lwt versions = Database.Version.get_all () >|=| List.map (fun v -> Version v) in
  (Lwt.return (persons @ dances @ books @ sets @ tunes @ versions))

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers: []

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold:(o A.threshold)
      (a A.filter)
  )

let search' ?pagination ?threshold filter =
  Lwt.map snd @@ search ?pagination ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let search_context ?threshold filter element =
  let%lwt results = search' ?threshold filter in
  let List.{total; previous; index; next; _} = Option.get @@ List.find_context (equal element) results in
  (* TODO: Return the context directly. *)
  Lwt.return (total, previous, index, next)

let () =
  Madge_server.(
    register ~endpoint:E.search_context @@ fun {a} {o} ->
    search_context
      ?threshold:(o A.threshold)
      (a A.filter)
      (a A.element)
  )
