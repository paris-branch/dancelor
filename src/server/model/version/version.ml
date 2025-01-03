open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include VersionLifted

module E = Common.Model.VersionEndpoints
module A = E.Arguments

let make_and_save
    ?status
    ~tune
    ~bars
    ~key
    ~structure
    ?arrangers
    ?remark
    ?disambiguation
    ?broken
    ~content
    ~modified_at
    ~created_at
    ()
  =
  let name = Tune.name tune in
  let%lwt version =
    Database.Version.save ~slug_hint: name @@ fun slug ->
    make
      ~slug
      ?status
      ~tune
      ~bars
      ~key
      ~structure
      ?arrangers
      ?remark
      ?disambiguation
      ?broken
      ~modified_at
      ~created_at
      ()
  in
  Database.Version.write_content version content;%lwt
  Lwt.return version

let () =
  Madge_server.(
    register ~endpoint: E.make_and_save @@ fun {a} {o} ->
    make_and_save
      ?status: (o A.status)
      ~tune: (a A.tune)
      ~bars: (a A.bars)
      ~key: (a A.key)
      ~structure: (a A.structure)
      ?arrangers: (o A.arrangers)
      ?remark: (o A.remark)
      ?disambiguation: (o A.disambiguation)
      ?broken: (o A.broken)
      ~content: (a A.content)
      ~modified_at: (a A.modified_at)
      ~created_at: (a A.created_at)
      ()
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
  | Not_found | Invalid_argument _ -> rem, acc

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

let tiebreakers =
  Lwt_list.[
    increasing (Lwt.map Tune.name % tune) String.Sensible.compare
  ]

let search =
  Search.search
    ~cache: (Cache.create ~lifetime: 600 ())
    ~values_getter: Database.Version.get_all
    ~scoring_function: Filter.accepts
    ~tiebreakers

let () =
  Madge_server.(
    register ~endpoint: E.search @@ fun {a} {o} ->
    search
      ?slice: (o A.slice)
      ?threshold: (o A.threshold)
      (a A.filter)
  )

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let mark_fixed version =
  Database.Version.update (set_broken version false)

let () =
  Madge_server.(
    register ~endpoint: E.mark_fixed @@ fun {a} _ ->
    mark_fixed (a A.version)
  )

let mark_broken version =
  Database.Version.update (set_broken version true)

let () =
  Madge_server.(
    register ~endpoint: E.mark_broken @@ fun {a} _ ->
    mark_broken (a A.version)
  )
