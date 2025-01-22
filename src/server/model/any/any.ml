open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include AnyLifted

let get_all () =
  let%lwt persons = Database.Person.get_all () >|=| List.map (fun c -> Person c) in
  let%lwt dances = Database.Dance.get_all () >|=| List.map (fun d -> Dance d) in
  let%lwt books = Database.Book.get_all () >|=| List.map (fun b -> Book b) in
  let%lwt sets = Database.Set.get_all () >|=| List.map (fun s -> Set s) in
  let%lwt tunes = Database.Tune.get_all () >|=| List.map (fun t -> Tune t) in
  let%lwt versions = Database.Version.get_all () >|=| List.map (fun v -> Version v) in
  (Lwt.return (persons @ dances @ books @ sets @ tunes @ versions))

let tiebreaker =
  curry @@ function
  | Person p1, Person p2 -> Lwt_list.compare_multiple Person.tiebreakers p1 p2
  | Dance d1, Dance d2 -> Lwt_list.compare_multiple Dance.tiebreakers d1 d2
  | Book b1, Book b2 -> Lwt_list.compare_multiple Book.tiebreakers b1 b2
  | Set s1, Set s2 -> Lwt_list.compare_multiple Set.tiebreakers s1 s2
  | Tune t1, Tune t2 -> Lwt_list.compare_multiple Tune.tiebreakers t1 t2
  | Version v1, Version v2 -> Lwt_list.compare_multiple Version.tiebreakers v1 v2
  | a1, a2 -> Lwt.return (Type.compare (type_of a1) (type_of a2))

let search =
  Common.Model.Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers: [tiebreaker]

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let search_context ?threshold filter element =
  let%lwt results = search' ?threshold filter in
  let List.{total; previous; index; next; _} = Option.get @@ List.find_context (equal element) results in
  (* TODO: Return the context directly. *)
  Lwt.return (total, previous, index, next)
