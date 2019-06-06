open NesLwt.Syntax
include Dancelor_common_model.Tune

let group t =
  let%lwt group = group t in
  Dancelor_server_database.TuneGroup.get group

let arranger t =
  match%lwt arranger t with
  | None -> Lwt.return_none
  | Some arranger ->
    let%lwt arranger = Dancelor_server_database.Credit.get arranger in
    Lwt.return_some arranger

let sources = sources >=>|| Lwt_list.map_s Source.get

let dances t =
  let%lwt dances = dances t in
  Lwt_list.map_s Dancelor_server_database.Dance.get dances

let content = Dancelor_server_database.Tune.read_content

(* * *)

let get = Dancelor_server_database.Tune.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let apply_filter filter all =
  let%lwt f_group_author = TuneFilter.group_author filter in
  let%lwt f_group_kind = TuneFilter.group_kind filter in
  let%lwt f_key = TuneFilter.key filter in
  let%lwt f_bars = TuneFilter.bars filter in
  Lwt_list.filter_s
    (fun tune ->
       let%lwt group = group tune in
       let%lwt group_author = TuneGroup.author group in
       let%lwt group_kind = TuneGroup.kind group in
       let%lwt key = key tune in
       let%lwt bars = bars tune in
       Lwt.return
         (
           (f_group_author = [] || (match group_author with None -> true | Some group_author -> List.mem group_author f_group_author))
           && (f_group_kind = [] || List.mem group_kind f_group_kind)
           && (f_key = [] || List.mem key f_key)
           && (f_bars = [] || List.mem bars f_bars)
         ))
    all

let apply_filter_on_scores filter all =
  let%lwt f_group_author = TuneFilter.group_author filter in
  let%lwt f_group_kind = TuneFilter.group_kind filter in
  let%lwt f_key = TuneFilter.key filter in
  let%lwt f_bars = TuneFilter.bars filter in
  Score.list_filter
    (fun tune ->
       let%lwt group = group tune in
       let%lwt group_author = TuneGroup.author group in
       let%lwt group_kind = TuneGroup.kind group in
       let%lwt key = key tune in
       let%lwt bars = bars tune in
       Lwt.return
         (
           (f_group_author = [] || (match group_author with None -> true | Some group_author -> List.mem group_author f_group_author))
           && (f_group_kind = [] || List.mem group_kind f_group_kind)
           && (f_key = [] || List.mem key f_key)
           && (f_bars = [] || List.mem bars f_bars)
         ))
    all

let all ?filter ?pagination () =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  let%lwt all =
    match filter with
    | Some filter -> apply_filter filter all
    | None -> Lwt.return all
  in
  let all = List.sort (fun t1 t2 -> compare (slug t1) (slug t2)) all in (* FIXME: it just works, but we're comparing promises here *)
  let%lwt all =
    match pagination with
    | Some pagination -> Pagination.apply pagination all
    | None -> Lwt.return all
  in
  Lwt.return all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.all @@ fun _ {o} ->
    all ?filter:(o Arg.filter) ?pagination:(o Arg.pagination) ()
  )

(* FIXME: ça pue *)

open Nes

let match_score needle haystack =
  let needle = Slug.from_string needle in
  let haystack = Slug.from_string haystack in
  NesString.inclusion_proximity ~needle haystack

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
    Lwt.return ((List.fold_left (+.) 0. scores) /. l)

let search ?filter ?pagination ?(threshold=0.) query =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  let all = Score.list_from_values all in
  let%lwt all = Score.list_map_score (search_against_tune query) all in
  let%lwt all =
    match filter with
    | Some filter -> apply_filter_on_scores filter all
    | None -> Lwt.return all
  in
  let all = Score.list_filter_threshold threshold all in
  let all =
    Score.list_sort_decreasing
      (fun tune1 tune2 -> compare (slug tune1) (slug tune2))
      all
  in
  let%lwt all =
    match pagination with
    | Some pagination -> Pagination.apply pagination all
    | None -> Lwt.return all
  in
  Lwt.return all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?filter:    (o Arg.filter)
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.terms)
  )
