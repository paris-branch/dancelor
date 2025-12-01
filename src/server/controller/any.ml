open Nes
open Common

let get env id =
  match Database.Any.get id with
  | None -> Permission.reject_can_get ()
  | Some any ->
    Permission.assert_can_get env (Model.Any.to_entry any);%lwt
    lwt any

include Search.Build(struct
  type value = Model.Any.t
  type filter = Filter.Any.t

  let get_all env =
    let%lwt sources = List.map Model.Any.source <$> Source.get_all env in
    let%lwt persons = List.map Model.Any.person <$> Person.get_all env in
    let%lwt dances = List.map Model.Any.dance <$> Dance.get_all env in
    let%lwt books = List.map Model.Any.book <$> Book.get_all env in
    let%lwt sets = List.map Model.Any.set <$> Set.get_all env in
    let%lwt tunes = List.map Model.Any.tune <$> Tune.get_all env in
    let%lwt versions = List.map Model.Any.version <$> Version.get_all env in
    lwt (sources @ persons @ dances @ books @ sets @ tunes @ versions)

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
