open Nes
open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let description ?link version =
  let%lwt bars = M.Version.bars version in
  let%lwt structure = M.Version.structure version in
  let%lwt key = M.Version.key version in
  let shape = spf "%d-bar %s version in %s" bars structure (M.Music.key_to_pretty_string key) in
  let%lwt arranger_block =
    match%lwt M.Version.arranger version with
    | None -> Lwt.return_nil
    | Some arranger ->
      let%lwt line_block = Credit.line ?link (Some arranger) in
      Lwt.return ([txt " arranged by "] @ line_block)
  in
  let%lwt disambiguation_block =
    match%lwt M.Version.disambiguation version with
    | "" -> Lwt.return_nil
    | disambiguation ->
      Lwt.return [txt (spf " (%s)" disambiguation)]
  in
  Lwt.return ([txt shape] @ arranger_block @ disambiguation_block)

let name ?(link=true) version =
  let name_text = [L.txt (M.Version.name version)] in
  if link then
    let href =
      let%lwt slug = M.Version.slug version in
      Lwt.return PageRouter.(path (Version slug))
    in
    Lwt.return [a ~a:[L.a_href href] name_text]
  else
    Lwt.return name_text

let name_and_dance ?link ?dance_link version parameters =
  let%lwt name = name ?link version in
  let%lwt dance =
    match%lwt M.VersionParameters.for_dance parameters with
    | None -> Lwt.return_nil
    | Some dance -> Lwt.return [
        span ~a:[a_class ["dim"; "details"]] [
          txt "For dance: ";
          L.span (Dance.name ?link:dance_link dance);
        ]]
  in
  Lwt.return (name @ dance)

let name_and_disambiguation ?link version =
  let%lwt name_block = name ?link version in
  let%lwt disambiguation_block =
    match%lwt M.Version.disambiguation version with
    | "" -> Lwt.return_nil
    | disambiguation -> Lwt.return [
        span ~a:[a_class ["dim"]] [txt (spf " (%s)" disambiguation)]
      ]
  in
  Lwt.return (name_block @ disambiguation_block)

let name_disambiguation_and_sources ?link version =
  let sources_lwt =
    let%lwt sources =
      let filter = M.Book.Filter.(
          M.Formula.and_ (memVersionDeep version) isSource
        )
      in
      M.Book.search filter
      >|=| M.Score.list_erase
    in
    match%lwt Lwt_list.map_p Book.short_title sources with
    | [] -> Lwt.return_nil
    | [title] -> Lwt.return (txt "Source: " :: title)
    | titles ->
      titles
      |> List.intertwine (fun _ -> [txt " - "])
      |> List.flatten
      |> List.cons (txt "Sources: ")
      |> Lwt.return
  in
  let%lwt name_and_disambiguation = name_and_disambiguation ?link version in
  Lwt.return (
    name_and_disambiguation
    @ [L.span ~a:[a_class ["dim"; "details"]] sources_lwt]
  )

let disambiguation_and_sources version =
  let sources_lwt =
    let%lwt sources =
      let filter = M.Book.Filter.(
          M.Formula.and_ (memVersionDeep version) isSource
        )
      in
      M.Book.search filter
      >|=| M.Score.list_erase
    in
    match%lwt Lwt_list.map_p Book.short_title sources with
    | [] -> Lwt.return_nil
    | [title] -> Lwt.return (txt "Source: " :: title)
    | titles ->
      titles
      |> List.intertwine (fun _ -> [txt " - "])
      |> List.flatten
      |> List.cons (txt "Sources: ")
      |> Lwt.return
  in
  Lwt.return [
    L.txt (M.Version.disambiguation version);
    L.span ~a:[a_class ["dim"; "details"]] sources_lwt;
  ]

let author_and_arranger ?(short=true) ?link version =
  let%lwt author_block =
    let%lwt tune = M.Version.tune version in
    match%lwt M.Tune.author tune with
    | None -> Lwt.return_nil
    | Some author -> Credit.line ?link (Some author)
  in
  let has_author =
    let%lwt tune = M.Version.tune version in
    match%lwt M.Tune.author tune with
    | None -> Lwt.return_false
    | Some _ -> Lwt.return_true
  in
  let%lwt arranger_block =
    match%lwt M.Version.arranger version with
    | None -> Lwt.return_nil
    | Some arranger ->
      let%lwt comma = if%lwt has_author then Lwt.return ", " else Lwt.return "" in
      let arr = if short then "arr." else "arranged by" in
      let%lwt arranger_block = Credit.line ?link (Some arranger) in
      Lwt.return [
        span ~a:[a_class ["dim"]] (txt (spf "%s%s " comma arr) :: arranger_block)
      ]
  in
  Lwt.return (author_block @ arranger_block)
