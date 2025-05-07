open Nes
open Common

open Html

let description ?link version =
  let bars = Model.Version.bars' version in
  let structure = Model.Version.structure' version in
  let key = Model.Version.key' version in
  let shape = spf "%d-bar %s version in %s" bars structure (Music.key_to_pretty_string key) in
  let%lwt arranger_block =
    match%lwt Model.Version.arrangers' version with
    | [] -> Lwt.return_nil
    | arrangers ->
      let name_block = Person.names ?link arrangers in
      Lwt.return ([txt " arranged by "] @ name_block)
  in
  let disambiguation_block =
    match Model.Version.disambiguation' version with
    | "" -> []
    | disambiguation -> [txt (spf " (%s)" disambiguation)]
  in
  Lwt.return ([txt shape] @ arranger_block @ disambiguation_block)

let name ?(link = true) version =
  let name_text = [L.txt (Model.Version.name' version)] in
  if link then
    [
      a
        ~a: [a_href @@ Endpoints.Page.href_version @@ Entry.slug version]
        name_text
    ]
  else
    name_text

let name_and_dance ?link ?dance_link version parameters =
  let%lwt dance =
    match%lwt Model.VersionParameters.for_dance parameters with
    | None -> Lwt.return_nil
    | Some dance ->
      Lwt.return
        [
          br ();
          small
            ~a: [a_class ["opacity-50"]]
            [
              txt "For dance: ";
              span (Dance.name ?link: dance_link dance);
            ]
        ]
  in
  Lwt.return (name ?link version @ dance)

let name_and_disambiguation ?link version =
  let disambiguation_block =
    match Model.Version.disambiguation' version with
    | "" -> []
    | disambiguation -> [span ~a: [a_class ["opacity-50"]] [txt (spf " (%s)" disambiguation)]]
  in
  Lwt.return (name ?link version @ disambiguation_block)

let name_disambiguation_and_sources ?link version =
  let sources_lwt =
    let%lwt sources =
      Lwt.map snd @@
        Madge_client.call_exn
          Endpoints.Api.(route @@ Book Search)
          Slice.everything
          Model.Book.Filter.(
            Formula.and_ (memVersionDeep' version) isSource'
          )
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
  Lwt.return
    (
      name_and_disambiguation @
        [L.span ~a: [a_class ["opacity-50"]] sources_lwt]
    )

let disambiguation_and_sources version =
  let sources_lwt =
    let%lwt sources =
      Lwt.map snd @@
        Madge_client.call_exn
          Endpoints.Api.(route @@ Book Search)
          Slice.everything
          Model.Book.Filter.(
            Formula.and_ (memVersionDeep' version) isSource'
          )
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
  Lwt.return
    [
      txt (Model.Version.disambiguation' version);
      L.span ~a: [a_class ["opacity-50"]] sources_lwt;
    ]

let composer_and_arranger ?(short = false) ?link version =
  let%lwt composer_block = Lwt.bind (Model.Version.tune' version) (Tune.composers ~short) in
  let%lwt arranger_block =
    match%lwt Model.Version.arrangers' version with
    | [] -> Lwt.return_nil
    | arrangers ->
      let comma = if composer_block <> [] then ", " else "" in
      let arr = if short then "arr." else "arranged by" in
      let arranger_block = Person.names ~short ?link arrangers in
      Lwt.return
        [
          span ~a: [a_class ["opacity-50"]] (txt (spf "%s%s " comma arr) :: arranger_block)
        ]
  in
  Lwt.return (composer_block @ arranger_block)
