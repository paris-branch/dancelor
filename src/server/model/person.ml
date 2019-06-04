include Dancelor_common_model.Person

(* * *)

let get = Dancelor_server_database.Person.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let make_and_save ~name () =
  Dancelor_server_database.Person.save ~slug_hint:name @@ fun slug ->
  Lwt.return (make ~slug ~name)

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} _ ->
    make_and_save
      ~name:(a Arg.name)
      ()
  )

let match_score needle haystack =
  let open Nes in
  let needle = Slug.from_string needle in
  let haystack = Slug.from_string haystack in
  let n =
    1. -.
    if String.length needle = 0 then
      0.
    else
      let d = String.inclusion_distance needle haystack in
      (float_of_int d) /. (float_of_int (String.length needle))
  in
  n *. n

let search_string_against_person term person =
  if String.length term < 3 then
    Lwt.return 0.
  else if term.[1] = ':' then
    Lwt.return 0.
  else
    let%lwt name = name person in
    Lwt.return (match_score term name)

let search_against_person search person =
  match search with
  | [] ->
    Lwt.return 1.
  | search ->
    let%lwt scores =
      Lwt_list.map_s
        (fun term -> search_string_against_person term person)
        search
    in
    let l = float_of_int (List.length search) in
    Lwt.return ((List.fold_left (+.) 0. scores) /. l)

let search ?(threshold=0.) query =
  let%lwt all = Dancelor_server_database.Person.get_all () in
  let all = Score.list_from_values all in
  let%lwt all = Score.list_map_score (search_against_person query) all in
  let all = Score.list_filter_threshold threshold all in
  let all =
    Score.list_sort_decreasing
      (fun person1 person2 -> compare (slug person1) (slug person2))
      all
  in
  Lwt.return all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search ?threshold:(o Arg.threshold)
      (a Arg.terms)
  )
