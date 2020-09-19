open Nes
include Dancelor_common_model.Version

let group = group >=>| Tune.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
let sources = sources >=>| Lwt_list.map_s Source.get

let content = Dancelor_server_database.Version.read_content

(* * *)

let get = Dancelor_server_database.Version.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

(* FIXME: merge apply_filter and apply_filter_on_scores *)

let apply_filter filter all =
  let%lwt f_group = VersionFilter.group filter in
  let%lwt f_group_author = VersionFilter.group_author filter in
  let%lwt f_group_kind = VersionFilter.group_kind filter in
  let%lwt f_key = VersionFilter.key filter in
  let%lwt f_bars = VersionFilter.bars filter in
  Lwt_list.filter_s
    (fun version ->
       let%lwt group = group version in
       let%lwt group_slug = Tune.slug group in
       let%lwt group_author = Tune.author group in
       let%lwt group_kind = Tune.kind group in
       let%lwt key = key version in
       let%lwt bars = bars version in
       Lwt.return
         (
           (f_group = [] || List.mem group_slug f_group)
           && (f_group_author = [] || (match group_author with None -> true | Some group_author -> List.mem group_author f_group_author))
           && (f_group_kind = [] || List.mem group_kind f_group_kind)
           && (f_key = [] || List.mem key f_key)
           && (f_bars = [] || List.mem bars f_bars)
         ))
    all

let apply_filter_on_scores filter all =
  let%lwt f_group = VersionFilter.group filter in
  let%lwt f_group_author = VersionFilter.group_author filter in
  let%lwt f_group_kind = VersionFilter.group_kind filter in
  let%lwt f_key = VersionFilter.key filter in
  let%lwt f_bars = VersionFilter.bars filter in
  Score.list_filter (* FIXME: this is literally the only difference between this function and the previous one *)
    (fun version ->
       let%lwt group = group version in
       let%lwt group_slug = Tune.slug group in
       let%lwt group_author = Tune.author group in
       let%lwt group_kind = Tune.kind group in
       let%lwt key = key version in
       let%lwt bars = bars version in
       Lwt.return
         (
           (f_group = [] || List.mem group_slug f_group)
           && (f_group_author = [] || (match group_author with None -> true | Some group_author -> List.mem group_author f_group_author))
           && (f_group_kind = [] || List.mem group_kind f_group_kind)
           && (f_key = [] || List.mem key f_key)
           && (f_bars = [] || List.mem bars f_bars)
         ))
    all

let all ?filter ?pagination () =
  Dancelor_server_database.Version.get_all ()
  >>=| Option.unwrap_map_or Lwt.return apply_filter filter
  >>=| Lwt_list.proj_sort_s ~proj:slug compare (* FIXME: We shouldn't sort wrt. slugs. *)
  >>=| Option.unwrap_map_or Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.all @@ fun _ {o} ->
    all ?filter:(o Arg.filter) ?pagination:(o Arg.pagination) ()
  )

let rec search_and_extract acc s regexp =
  let rem = Str.replace_first regexp "" s in
  try
    let gp = Str.matched_group 1 s in
    let gp_words =
      String.split_on_char ',' gp
      |> List.map (String.remove_char '"')
      |> List.map (String.remove_char '\'')
      |> List.filter (fun s -> s <> "")
    in
    let rem, l = search_and_extract acc rem regexp in
    rem, gp_words @ l
  with
    Not_found | Invalid_argument _ -> rem, acc

let extract_search_option s opt =
  let dquote_regex = Printf.sprintf "%s:\"\\([^\"]*\\)\"" opt |> Str.regexp in
  let squote_regex = Printf.sprintf "%s:'\\([^']*\\)'" opt |> Str.regexp in
  let simple_regex = Printf.sprintf "%s:\\([^ ]*\\)" opt |> Str.regexp in
  let rem, l = search_and_extract [] s dquote_regex in
  let rem, l = search_and_extract l rem squote_regex in
  search_and_extract l rem simple_regex

let parse_filter_kind k =
  try
    Some (Kind.base_of_string k)
  with
    Failure _ -> None

let parse_filter_key k =
  try
    Some (Music.key_of_string k)
  with
    Failure _ -> None

let extract_search s =
  let rem, kinds = extract_search_option s "kind" in
  let rem, keys = extract_search_option rem "key" in
  let rem, authors = extract_search_option rem "author" in
  let words =
    String.split_on_char ' ' rem
    |> List.map (String.remove_char '"')
    |> List.map (String.remove_char '\'')
    |> List.filter (fun s -> s <> "")
  in
  (
    List.filter_map parse_filter_kind kinds,
    authors,
    List.filter_map parse_filter_key keys,
    String.concat " " words
  )

let score_list_vs_word words needle =
  List.map (String.sensible_inclusion_proximity ~needle) words
  |> List.fold_left max 0.

let score_list_vs_list words needles =
  if needles = [] then 1.
  else begin
    List.map (score_list_vs_word words) needles
    |> List.fold_left max 0.
  end

let score ~kinds ~authors ~keys words version =
  let%lwt group = group version in
  let%lwt key = key version in
  let%lwt kind = Tune.kind group in
  let%lwt name = Tune.name group in
  let%lwt alt_names = Tune.alt_names group in
  let%lwt credit = Tune.author group in
  let%lwt credit_words =
    match credit with
    | None -> Lwt.return []
    | Some credit ->
      let%lwt author = Credit.line credit in
      let%lwt persons = Credit.persons credit in
      let%lwt names = Lwt_list.map_s Person.name persons in
      Lwt.return (author :: names)
  in
  let version_words = name::alt_names in
  if (keys <> [] && not (List.mem key keys))
  || (kinds <> [] && not (List.mem kind kinds)) then
    Lwt.return 0.
  else begin
    let authors_score = score_list_vs_list credit_words authors in
    let words_score = score_list_vs_word version_words words in
    Lwt.return (authors_score *. words_score)
  end

let search ?filter ?pagination ?(threshold=0.) string =
  let kinds, authors, keys, words = extract_search string in
  Dancelor_server_database.Version.get_all ()
  >>=| Score.lwt_map_from_list (score ~kinds ~authors ~keys words)
  >>=| Option.unwrap_map_or Lwt.return apply_filter_on_scores filter
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:(group >=>| Tune.name) String.sensible_compare
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
