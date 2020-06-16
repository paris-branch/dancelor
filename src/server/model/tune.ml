open Nes
include Dancelor_common_model.Tune

let group = group >=>| TuneGroup.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
let sources = sources >=>| Lwt_list.map_s Source.get

let content = Dancelor_server_database.Tune.read_content

(* * *)

let get = Dancelor_server_database.Tune.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

(* FIXME: merge apply_filter and apply_filter_on_scores *)

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
  Dancelor_server_database.Tune.get_all ()
  >>=| Option.unwrap_map_or Lwt.return apply_filter filter
  >>=| Lwt_list.proj_sort_s ~proj:slug compare (* FIXME: We shouldn't sort wrt. slugs. *)
  >>=| Option.unwrap_map_or Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.all @@ fun _ {o} ->
    all ?filter:(o Arg.filter) ?pagination:(o Arg.pagination) ()
  )

let rec extract_words = function
  | [] -> [], []
  | h::t ->
    let kinds, words = extract_words t in
    let add_kind k =
      if not (List.mem k kinds) then
        (k::kinds, words)
      else
        (kinds, words)
    in
    let h = String.uncapitalize h in
    if h = "j" || h = "jig" then
      add_kind Kind.Jig
    else if h = "r" || h = "reel" then
      add_kind Kind.Reel
    else if h = "s" || h = "strathspey" then
      add_kind Kind.Strathspey
    else if h = "w" || h = "waltz" then
      add_kind Kind.Waltz
    else if h = "p" || h = "polka" then
      add_kind Kind.Polka
    else if h = "" then
      (kinds, words)
    else
      (kinds, h::words)

let score_list words list =
  List.map (fun needle ->
    List.map (String.sensible_inclusion_proximity ~needle) list
    |> List.fold_left max 0.) words
  |> List.fold_left (fun (acc, n) v -> (acc +. v, n +. 1.)) (0., 0.)
  |> fun (sum, n) -> if n = 0. then 0. else (sum /. n)

let search kinds words tune =
  let%lwt group = group tune in
  let%lwt name = TuneGroup.name group in
  let%lwt alt_names = TuneGroup.alt_names group in
  let%lwt credit = TuneGroup.author group in
  let%lwt tune_words =
    match credit with
    | None ->
      Lwt.return (name :: alt_names)
    | Some credit ->
      let%lwt author = Credit.line credit in
      let%lwt persons = Credit.persons credit in
      let%lwt names = Lwt_list.map_s Person.name persons in
      Lwt.return ((name :: alt_names) @ (author :: names))
  in
  let words_score = score_list words tune_words in
  let%lwt kind = TuneGroup.kind group in
  let kind_multiplier =
    if kinds = [] then 1.
    else if List.mem kind kinds then 1.
    else 0.5
  in
  Lwt.return (kind_multiplier *. words_score)

let search ?filter ?pagination ?(threshold=0.) string =
  let kinds, words = extract_words (String.split_on_char ' ' string) in
  Dancelor_server_database.Tune.get_all ()
  >>=| Score.lwt_map_from_list (search kinds words)
  >>=| Option.unwrap_map_or Lwt.return apply_filter_on_scores filter
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:(group >=>| TuneGroup.name) String.sensible_compare
  >>=| Option.unwrap_map_or Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?filter:    (o Arg.filter)
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )

let count ?filter () =
  let%lwt l = all ?filter () in
  Lwt.return (List.length l)

let () =
  Madge_server.(
    register ~endpoint:Endpoint.count @@ fun _ {o} ->
    count
      ?filter:(o Arg.filter) ()
  )
