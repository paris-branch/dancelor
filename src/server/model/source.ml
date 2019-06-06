include Dancelor_common_model.Source

(* * *)

let get = Dancelor_server_database.Source.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let make_and_save ~name () =
  Dancelor_server_database.Source.save ~slug_hint:name @@ fun slug ->
  Lwt.return (make ~slug ~name)

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun {a} _ ->
    make_and_save
      ~name:(a Arg.name)
      ()
  )

let match_score needle haystack =
  let needle = NesSlug.from_string needle in
  let haystack = NesSlug.from_string haystack in
  NesString.inclusion_proximity ~needle haystack

let search_string_against_source term source =
  if String.length term < 3 then
    Lwt.return 0.
  else if term.[1] = ':' then
    Lwt.return 0.
  else
    let%lwt name = name source in
    Lwt.return (match_score term name)

let search_against_source search source =
  match search with
  | [] ->
    Lwt.return 1.
  | search ->
    let%lwt scores =
      Lwt_list.map_s
        (fun term -> search_string_against_source term source)
        search
    in
    let l = float_of_int (List.length search) in
    Lwt.return ((List.fold_left (+.) 0. scores) /. l)

let search ?(threshold=0.) query =
  let%lwt all = Dancelor_server_database.Source.get_all () in
  let all = Score.list_from_values all in
  let%lwt all = Score.list_map_score (search_against_source query) all in
  let all = Score.list_filter_threshold threshold all in
  let all =
    Score.list_sort_decreasing
      (fun source1 source2 -> compare (slug source1) (slug source2))
      all
  in
  Lwt.return all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search ?threshold:(o Arg.threshold)
      (a Arg.terms)
  )
