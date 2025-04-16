open Nes
open Common

include ModelBuilder.Search.Build(struct
  type value = Model.Any.t
  type filter = Model.Any.Filter.t

  let cache = Cache.create ~lifetime: 600 ()

  let get_all () =
    let%lwt sources = Database.Source.get_all () >|=| List.map (fun c -> Model.Any.Source c) in
    let%lwt persons = Database.Person.get_all () >|=| List.map (fun c -> Model.Any.Person c) in
    let%lwt dances = Database.Dance.get_all () >|=| List.map (fun d -> Model.Any.Dance d) in
    let%lwt books = Database.Book.get_all () >|=| List.map (fun b -> Model.Any.Book b) in
    let%lwt sets = Database.Set.get_all () >|=| List.map (fun s -> Model.Any.Set s) in
    let%lwt tunes = Database.Tune.get_all () >|=| List.map (fun t -> Model.Any.Tune t) in
    let%lwt versions = Database.Version.get_all () >|=| List.map (fun v -> Model.Any.Version v) in
      (Lwt.return (sources @ persons @ dances @ books @ sets @ tunes @ versions))

  let filter_accepts = Model.Any.Filter.accepts

  let tiebreakers = [
    curry @@ function
      | Model.Any.Source p1, Model.Any.Source p2 -> Lwt_list.compare_multiple Source.tiebreakers p1 p2
      | Person p1, Person p2 -> Lwt_list.compare_multiple Person.tiebreakers p1 p2
      | Dance d1, Dance d2 -> Lwt_list.compare_multiple Dance.tiebreakers d1 d2
      | Book b1, Book b2 -> Lwt_list.compare_multiple Book.tiebreakers b1 b2
      | Set s1, Set s2 -> Lwt_list.compare_multiple Set.tiebreakers s1 s2
      | Tune t1, Tune t2 -> Lwt_list.compare_multiple Tune.tiebreakers t1 t2
      | Version v1, Version v2 -> Lwt_list.compare_multiple Version.tiebreakers v1 v2
      | a1, a2 -> Lwt.return Model.Any.(Type.compare (type_of a1) (type_of a2))
  ]
end)

let search_context filter element =
  let%lwt results = search' filter in
  let List.{total; previous; index; next; _} = Option.get @@ List.find_context (Model.Any.equal element) results in
  (* TODO: Return the context directly. *)
  Lwt.return (total, previous, index, next)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Any.t -> a = fun _env endpoint ->
  match endpoint with
  | Search -> search
  | SearchContext -> search_context
