open Nes
open Dancelor_common
open Dancelor_client_html
module M = Dancelor_client_model

let description ?link version =
  let bars = M.Version.bars version in
  let structure = M.Version.structure version in
  let key = M.Version.key version in
  let shape = spf "%d-bar %s version in %s" bars structure (M.Music.key_to_pretty_string key) in
  let%lwt arranger_block =
    match%lwt M.Version.arrangers version with
    | [] -> Lwt.return_nil
    | arrangers ->
      let name_block = Person.names ?link arrangers in
      Lwt.return ([txt " arranged by "] @ name_block)
  in
  let disambiguation_block =
    match M.Version.disambiguation version with
    | "" -> []
    | disambiguation -> [txt (spf " (%s)" disambiguation)]
  in
  Lwt.return ([txt shape] @ arranger_block @ disambiguation_block)

let name ?(link=true) version =
  let name_text = [L.txt (M.Version.name version)] in
  if link then
    [
      a
        ~a:[a_href @@ PageRouter.path_version @@ M.Version.slug version]
        name_text
    ]
  else
    name_text

let name_and_dance ?link ?dance_link version parameters =
  let%lwt dance =
    match%lwt M.VersionParameters.for_dance parameters with
    | None -> Lwt.return_nil
    | Some dance -> Lwt.return [
        span ~a:[a_class ["dim"; "details"]] [
          txt "For dance: ";
          span (Dance.name ?link:dance_link dance);
        ]]
  in
  Lwt.return (name ?link version @ dance)

let name_and_disambiguation ?link version =
  let disambiguation_block =
    match M.Version.disambiguation version with
    | "" -> []
    | disambiguation -> [span ~a:[a_class ["dim"]] [txt (spf " (%s)" disambiguation)]]
  in
  Lwt.return (name ?link version @ disambiguation_block)

let name_disambiguation_and_sources ?link version =
  let sources_lwt =
    let%lwt sources =
      M.Book.(search' Filter.(
          M.Formula.and_ (memVersionDeep' version) isSource'
        ))
    in
    Lwt.return @@
    match List.map Book.short_title sources with
    | [] -> []
    | [title] -> txt "Source: " :: title
    | titles ->
      titles
      |> List.interspersei (fun _ -> [txt " - "])
      |> List.flatten
      |> List.cons (txt "Sources: ")
  in
  let%lwt name_and_disambiguation = name_and_disambiguation ?link version in
  Lwt.return (
    name_and_disambiguation
    @ [L.span ~a:[a_class ["dim"; "details"]] sources_lwt]
  )

let disambiguation_and_sources version =
  let sources_lwt =
    let%lwt sources =
      M.Book.(search' Filter.(
          M.Formula.and_ (memVersionDeep' version) isSource'
        ))
    in
    Lwt.return @@
    match List.map Book.short_title sources with
    | [] -> []
    | [title] -> txt "Source: " :: title
    | titles ->
      titles
      |> List.interspersei (fun _ -> [txt " - "])
      |> List.flatten
      |> List.cons (txt "Sources: ")
  in
  Lwt.return [
    txt (M.Version.disambiguation version);
    L.span ~a:[a_class ["dim"; "details"]] sources_lwt;
  ]

let composer_and_arranger ?(short=false) ?link version =
  let%lwt composer_block = Lwt.bind (M.Version.tune version) (Tune.composers ~short) in
  let%lwt arranger_block =
    match%lwt M.Version.arrangers version with
    | [] -> Lwt.return_nil
    | arrangers ->
      let comma = if composer_block <> [] then ", " else "" in
      let arr = if short then "arr." else "arranged by" in
      let arranger_block = Person.names ~short ?link arrangers in
      Lwt.return [
        span ~a:[a_class ["dim"]] (txt (spf "%s%s " comma arr) :: arranger_block)
      ]
  in
  Lwt.return (composer_block @ arranger_block)
