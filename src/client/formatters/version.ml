open Nes
open Common

open Html

let description ?arranger_links version =
  let bars = Model.Version.bars version in
  let structure = Model.Version.structure version in
  let key = Model.Version.key version in
  let shape = spf "%d-bar %s version in %s" bars structure (Music.key_to_pretty_string key) in
  let%lwt arranger_block =
    match%lwt Model.Version.arrangers version with
    | [] -> Lwt.return_nil
    | arrangers ->
      let name_block = Person.names' ?links: arranger_links arrangers in
      Lwt.return ([txt " arranged by "] @ name_block)
  in
  let disambiguation_block =
    match Model.Version.disambiguation version with
    | "" -> []
    | disambiguation -> [txt (spf " (%s)" disambiguation)]
  in
  Lwt.return ([txt shape] @ arranger_block @ disambiguation_block)

let description' ?arranger_links version =
  description ?arranger_links @@ Entry.value version

let name_gen = function
  | Right (version, true) ->
    a
      ~a: [a_href @@ Endpoints.Page.href_version @@ Entry.slug version]
      [L.txt @@ Model.Version.name' version]
  | Right (version, _) -> L.txt (Model.Version.name' version)
  | Left version -> L.txt (Model.Version.name version)

let name = name_gen % Either.left
let name' ?(link = true) version = name_gen @@ Right (version, link)

let name_and_dance_gen ?dance_link version parameters =
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
              Dance.name' ?link: dance_link dance;
            ]
        ]
  in
  Lwt.return (name_gen version :: dance)

let name_and_dance ?dance_link version parameters =
  name_and_dance_gen ?dance_link (Left version) parameters

let name_and_dance' ?(name_link = true) ?dance_link version parameters =
  name_and_dance_gen ?dance_link (Right (version, name_link)) parameters

let name_and_disambiguation_gen version =
  let disambiguation_block =
    match Model.Version.disambiguation @@ Either.fold ~left: Fun.id ~right: (Entry.value % fst) version with
    | "" -> []
    | disambiguation -> [span ~a: [a_class ["opacity-50"]] [txt (spf " (%s)" disambiguation)]]
  in
  Lwt.return (name_gen version :: disambiguation_block)

let name_and_disambiguation = name_and_disambiguation_gen % Either.left
let name_and_disambiguation' ?(name_link = true) version = name_and_disambiguation_gen @@ Right (version, name_link)

let name_disambiguation_and_sources' ?name_link version =
  (* FIXME: Use not books but actual sources. This should also avoid a search,
     and therefore allow splitting this formatter into a [_gen] one. *)
  let sources_lwt =
    let%lwt sources =
      Lwt.map snd @@
        Madge_client.call_exn
          Endpoints.Api.(route @@ Book Search)
          Slice.everything
          Filter.Book.(Formula.and_ (memVersionDeep' version) isSource')
    in
    Lwt.return @@
      match List.map Book.short_title' sources with
      | [] -> []
      | [title] -> txt "Source: " :: title
      | titles ->
        titles
        |> List.interspersei (fun _ -> [txt " - "])
        |> List.flatten
        |> List.cons (txt "Sources: ")
  in
  let%lwt name_and_disambiguation = name_and_disambiguation' ?name_link version in
  Lwt.return
    (
      name_and_disambiguation @
        [L.span ~a: [a_class ["opacity-50"]] sources_lwt]
    )

let disambiguation_and_sources' version =
  (* FIXME: same as above *)
  let sources_lwt =
    let%lwt sources =
      Lwt.map snd @@
        Madge_client.call_exn
          Endpoints.Api.(route @@ Book Search)
          Slice.everything
          Filter.Book.(Formula.and_ (memVersionDeep' version) isSource')
    in
    Lwt.return @@
      match List.map Book.short_title' sources with
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

let composer_and_arranger ?(short = false) ?arranger_links version =
  let%lwt composer_block = Lwt.bind (Model.Version.tune version) (Tune.composers' ~short) in
  let%lwt arranger_block =
    match%lwt Model.Version.arrangers version with
    | [] -> Lwt.return_nil
    | arrangers ->
      let comma = if composer_block <> [] then ", " else "" in
      let arr = if short then "arr." else "arranged by" in
      let arranger_block = Person.names' ~short ?links: arranger_links arrangers in
      Lwt.return
        [
          span ~a: [a_class ["opacity-50"]] (txt (spf "%s%s " comma arr) :: arranger_block)
        ]
  in
  Lwt.return (composer_block @ arranger_block)

let composer_and_arranger' ?short ?arranger_links version =
  composer_and_arranger ?short ?arranger_links (Entry.value version)
