open Nes
module E = Dancelor_common_model.Version_endpoints
module A = E.Arguments

include Dancelor_common_model.Version

let tune = tune >=>| Tune.get
let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
let sources = sources >=>| Lwt_list.map_s Source.get

let content = Dancelor_server_database.Version.read_content

module Filter = struct
  include Filter

  let accepts filter version =
    match filter with
    | Is version' ->
      let%lwt slug' = slug version' in
      let%lwt slug  = slug version  in
      Lwt.return (Slug.equal slug slug')
    | Tune tfilter ->
      let%lwt tune = tune version in
      Tune.Filter.accepts tfilter tune
    | Key key' ->
      let%lwt key = key version in
      Lwt.return (key = key')
    | Bars bars' ->
      let%lwt bars = bars version in
      Lwt.return (bars = bars')
end

let get = Dancelor_server_database.Version.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let apply_filter filter all =
  Lwt_list.filter_s (Formula.accepts Filter.accepts filter) all

let apply_filter_on_scores filter all =
  Score.list_filter_lwt (Formula.accepts Filter.accepts filter) all

let all ?filter ?pagination () =
  Dancelor_server_database.Version.get_all ()
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter filter
  >>=| Lwt_list.(sort_multiple [
      increasing (tune >=>| Tune.name) String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.all @@ fun _ {o} ->
    all ?filter:(o A.filter) ?pagination:(o A.pagination) ()
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
  let dquote_regex = spf "%s:\"\\([^\"]*\\)\"" opt |> Str.regexp in
  let squote_regex = spf "%s:'\\([^']*\\)'" opt |> Str.regexp in
  let simple_regex = spf "%s:\\([^ ]*\\)" opt |> Str.regexp in
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
  List.map (String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle) words
  |> List.fold_left max 0.

let score_list_vs_list words needles =
  if needles = [] then 1.
  else begin
    List.map (score_list_vs_word words) needles
    |> List.fold_left max 0.
  end

let score ~kinds ~authors ~keys words version =
  let%lwt tune = tune version in
  let%lwt key = key version in
  let%lwt kind = Tune.kind tune in
  let%lwt name = Tune.name tune in
  let%lwt alternative_names = Tune.alternative_names tune in
  let%lwt credit = Tune.author tune in
  let%lwt credit_words =
    match credit with
    | None -> Lwt.return []
    | Some credit ->
      let%lwt author = Credit.line credit in
      let%lwt persons = Credit.persons credit in
      let%lwt names = Lwt_list.map_s Person.name persons in
      Lwt.return (author :: names)
  in
  let version_words = name::alternative_names in
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
  >>=| Option.unwrap_map_or ~default:Lwt.return apply_filter_on_scores filter
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing (tune >=>| Tune.name) String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?filter:    (o A.filter)
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.string)
  )

let count ?filter () =
  let%lwt l = all ?filter () in
  Lwt.return (List.length l)

let () =
  Madge_server.(
    register ~endpoint:E.count @@ fun _ {o} ->
    count
      ?filter:(o A.filter) ()
  )
