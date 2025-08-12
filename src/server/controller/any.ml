open Nes
open Common

let get env id =
  match Database.Any.get id with
  | None ->
    Madge_server.shortcut_not_found "This entry does not exist, or you do not have access to it."
  | Some any ->
    (
      match any with
      | Source source -> Permission.assert_can_get env source
      | Person person -> Permission.assert_can_get env person
      | Dance dance -> Permission.assert_can_get env dance
      | Book book -> Permission.assert_can_get env book
      | Set set -> Permission.assert_can_get env set
      | Tune tune -> Permission.assert_can_get env tune
      | Version version -> Permission.assert_can_get env version
      | User user -> Permission.assert_can_get env user
    );%lwt
    lwt any

include Search.Build(struct
  type value = Model.Any.t
  type filter = Filter.Any.t

  let get_all env =
    let%lwt sources = Source.get_all env >|=| List.map (fun c -> Model.Any.Source c) in
    let%lwt persons = Person.get_all env >|=| List.map (fun c -> Model.Any.Person c) in
    let%lwt dances = Dance.get_all env >|=| List.map (fun d -> Model.Any.Dance d) in
    let%lwt books = Book.get_all env >|=| List.map (fun b -> Model.Any.Book b) in
    let%lwt sets = Set.get_all env >|=| List.map (fun s -> Model.Any.Set s) in
    let%lwt tunes = Tune.get_all env >|=| List.map (fun t -> Model.Any.Tune t) in
    let%lwt versions = Version.get_all env >|=| List.map (fun v -> Model.Any.Version v) in
      (lwt (sources @ persons @ dances @ books @ sets @ tunes @ versions))

  let filter_accepts = Filter.Any.accepts

  let tiebreakers = [
    curry @@ function
      | Model.Any.Source p1, Model.Any.Source p2 -> Lwt_list.compare_multiple Source.tiebreakers p1 p2
      | Person p1, Person p2 -> Lwt_list.compare_multiple Person.tiebreakers p1 p2
      | Dance d1, Dance d2 -> Lwt_list.compare_multiple Dance.tiebreakers d1 d2
      | Book b1, Book b2 -> Lwt_list.compare_multiple Book.tiebreakers b1 b2
      | Set s1, Set s2 -> Lwt_list.compare_multiple Set.tiebreakers s1 s2
      | Tune t1, Tune t2 -> Lwt_list.compare_multiple Tune.tiebreakers t1 t2
      | Version v1, Version v2 -> Lwt_list.compare_multiple Version.tiebreakers v1 v2
      | a1, a2 -> lwt Model.Any.(Type.compare (type_of a1) (type_of a2))
  ]
end)

let search_context env filter element =
  let%lwt results = snd <$> search env Slice.everything filter in
  let List.{total; previous; index; next; _} = Option.get @@ List.find_context (Model.Any.equal element) results in
  (* TODO: Return the context directly. *)
  lwt (total, previous, index, next)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Any.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | SearchContext -> search_context env
