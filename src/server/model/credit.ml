include Dancelor_common_model.Credit

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_s Dancelor_server_database.Person.get persons

(* * *)

let get = Dancelor_server_database.Credit.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let make_and_save ~line ?persons () =
  let%lwt persons =
    match persons with
    | None -> Lwt.return_none
    | Some persons ->
      let%lwt persons = Lwt_list.map_s Person.slug persons in
      Lwt.return_some persons
  in
  Dancelor_server_database.Credit.save ~slug_hint:line @@ fun slug ->
  Lwt.return (make ~slug ~line ?persons ())

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    make_and_save
      ~line:   (a Arg.line)
      ?persons:(o Arg.persons)
      ()
  )

let match_score needle haystack =
  let needle = NesSlug.from_string needle in
  let haystack = NesSlug.from_string haystack in
  NesString.inclusion_proximity ~needle haystack

let search_string_against_credit term credit =
  if String.length term < 3 then
    Lwt.return 0.
  else if term.[1] = ':' then
    Lwt.return 0.
  else
    let%lwt name = line credit in
    Lwt.return (match_score term name)

let search_against_credit search credit =
  match search with
  | [] ->
    Lwt.return 1.
  | search ->
    let%lwt scores =
      Lwt_list.map_s
        (fun term -> search_string_against_credit term credit)
        search
    in
    let l = float_of_int (List.length search) in
    Lwt.return ((List.fold_left (+.) 0. scores) /. l)

let search ?(threshold=0.) query =
  let%lwt all = Dancelor_server_database.Credit.get_all () in
  let all = Score.list_from_values all in
  let%lwt all = Score.list_map_score (search_against_credit query) all in
  let all = Score.list_filter_threshold threshold all in
  let all =
    Score.list_sort_decreasing
      (fun cr1 cr2 -> compare (slug cr1) (slug cr2))
      all
  in
  Lwt.return all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search ?threshold:(o Arg.threshold)
      (a Arg.terms)
  )
