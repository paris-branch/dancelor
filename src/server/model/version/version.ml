open Nes
include VersionLifted

module E = Dancelor_common_model.VersionEndpoints
module A = E.Arguments

let make_and_save
    ?status
    ~tune
    ~bars
    ~key
    ~structure
    ?arranger
    ?remark
    ?disambiguation
    ?broken
    ~content
    ()
  =
  let%lwt name = Tune.name tune in
  let%lwt version =
    Dancelor_server_database.Version.save ~slug_hint: name
    @@ fun slug ->
      make
        ~slug
        ?status
        ~tune
        ~bars
        ~key
        ~structure
        ?arranger
        ?remark
        ?disambiguation
        ?broken
        ()
  in
  Dancelor_server_database.Version.write_content version content;%lwt
  Lwt.return version

let () =
  Madge_server.(register ~endpoint: E.make_and_save
  @@ fun { a } { o } ->
    make_and_save
      ?status: (o A.status)
      ~tune: (a A.tune)
      ~bars: (a A.bars)
      ~key: (a A.key)
      ~structure: (a A.structure)
      ?arranger: (o A.arranger)
      ?remark: (o A.remark)
      ?disambiguation: (o A.disambiguation)
      ?broken: (o A.broken)
      ~content: (a A.content)
      ())

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

let score_list_vs_word words needle =
  List.map (String.inclusion_proximity ~char_equal: Char.Sensible.equal ~needle) words
  |> List.fold_left max 0.

let score_list_vs_list words needles =
  if needles = [] then 1.
  else
    begin
      List.map (score_list_vs_word words) needles
      |> List.fold_left max 0.
    end

let search ?pagination ?(threshold = Float.min_float) filter =
  Dancelor_server_database.Version.get_all ()
  >>=| Score.lwt_map_from_list (VersionFilter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing
    [
      increasing (tune >=>| Tune.name) String.Sensible.compare;
    ])
  >>=| Option.unwrap_map_or ~default: Lwt.return Pagination.apply pagination

let () =
  Madge_server.(register ~endpoint: E.search
  @@ fun { a } { o } ->
    search
      ?pagination: (o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter))

let count ?threshold filter =
  let%lwt l = search ?threshold filter in
  Lwt.return (List.length l)

let () =
  Madge_server.(register ~endpoint: E.count
  @@ fun { a } { o } ->
    count ?threshold: (o A.threshold) (a A.filter))

let mark_fixed version =
  let%lwt version = set_broken version false in
  Dancelor_server_database.Version.update version

let () =
  Madge_server.(register ~endpoint: E.mark_fixed
  @@ fun { a } _ ->
    mark_fixed (a A.version))

let mark_broken version =
  let%lwt version = set_broken version true in
  Dancelor_server_database.Version.update version

let () =
  Madge_server.(register ~endpoint: E.mark_broken
  @@ fun { a } _ ->
    mark_broken (a A.version))
