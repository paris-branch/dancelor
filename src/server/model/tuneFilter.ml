include Dancelor_common_model.TuneFilter

let make ?group_author ?group_kind ?key ?bars () =
  let%lwt group_author =
    match group_author with
    | None -> Lwt.return_none
    | Some group_author ->
      let%lwt group_author = Lwt_list.map_s Credit.slug group_author in
      Lwt.return_some group_author
  in
  make ?group_author ?group_kind ?key ?bars ()

let group_author f =
  let%lwt author = group_author f in
  Lwt_list.map_s Credit.get author

(* let match_score needle haystack =
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

   let search_meta_against_tune term tune =
   let%lwt group = group tune in
   let%lwt kind_score =
    let%lwt kind = TuneGroup.kind group in
    Lwt.return (if Kind.base_to_string kind = term then 1. else 0.25)
   in
   let%lwt structure_score =
    let%lwt structure = structure tune in
    Lwt.return (if structure = term then 1. else 0.25)
   in
   let%lwt bars_score =
    let%lwt bars = bars tune in
    Lwt.return (if string_of_int bars = term then 1. else 0.25)
   in
   let%lwt key_score =
    let%lwt key = key tune in
    Lwt.return (if Music.key_to_string key = term then 1. else 0.25)
   in
   max_l
    [ kind_score ;
      structure_score ;
      bars_score ;
      key_score ]
   |> Lwt.return

   let search_string_against_tune term tune =
   if String.length term < 3 then
    Lwt.return 0.
   else if term.[1] = ':' then
    Lwt.return 0.
   else
    let%lwt group = group tune in
    let%lwt name_score =
      let%lwt name = TuneGroup.name group in
      Lwt.return (match_score term name)
    in
    let%lwt author_score =
      match%lwt TuneGroup.author group with
      | None -> Lwt.return 0.
      | Some author ->
        let%lwt author = Credit.line author in
        Lwt.return (match_score term author)
    in
    Lwt.return (max name_score author_score)

   let search_against_tune search tune =
   match search with
   | [] ->
    Lwt.return 1.
   | search ->
    let%lwt scores =
      Lwt_list.map_s
        (fun term ->
           max
             (search_meta_against_tune term tune)
             (search_string_against_tune term tune))
        search
    in
    let l = float_of_int (List.length search) in
    Lwt.return ((List.fold_left (+.) 0. scores) /. l) *)
(*
let get_all ?(search=[]) ?(threshold=0.) ?(hard_limit=max_int) () =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  let all = Score.list_from_values all in
  let%lwt all = Score.list_map_score (search_against_tune search) all in
  let all = Score.list_filter_threshold threshold all in
  let all =
    Score.list_sort_decreasing
      (fun tune1 tune2 -> compare (slug tune1) (slug tune2))
      all
  in
  let all = List.sub hard_limit all in
  Lwt.return all (* FIXME: * 100 ? *) *)
