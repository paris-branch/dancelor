open Nes
module E = Dancelor_common_model.Version_endpoints
module A = E.Arguments

module Self = struct
  include Dancelor_common_model.Version

  let tune = tune >=>| Tune.get
  let arranger = arranger >=>?| (Credit.get >=>| Lwt.return_some)
  let sources = sources >=>| Lwt_list.map_s Source.get

  let content = Dancelor_server_database.Version.read_content
end
include Self

module Filter = struct
  include Filter

  let accepts filter version =
    Formula.interpret filter @@ function

    | Is version' ->
      equal version version' >|=| Formula.interpret_bool

    | Tune tfilter ->
      let%lwt tune = Self.tune version in
      Tune.Filter.accepts tfilter tune

    | Key key' ->
      let%lwt key = Self.key version in
      Lwt.return (Formula.interpret_bool (key = key'))

    | BarsEq bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars = bars'))
    | BarsNe bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars <> bars'))
    | BarsGt bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars > bars'))
    | BarsGe bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars >= bars'))
    | BarsLt bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars < bars'))
    | BarsLe bars' ->
      let%lwt bars = Self.bars version in
      Lwt.return (Formula.interpret_bool (bars <= bars'))
end

let get = Dancelor_server_database.Version.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
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

let score_list_vs_word words needle =
  List.map (String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle) words
  |> List.fold_left max 0.

let score_list_vs_list words needles =
  if needles = [] then 1.
  else begin
    List.map (score_list_vs_word words) needles
    |> List.fold_left max 0.
  end

let search ?pagination ?(threshold=0.) filter =
  Dancelor_server_database.Version.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing (tune >=>| Tune.name) String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let count filter =
  let%lwt l = search filter in
  Lwt.return (List.length l)

let () =
  Madge_server.(
    register ~endpoint:E.count @@ fun {a} _ ->
    count (a A.filter)
  )
