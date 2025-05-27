open Nes
open Common

open Html

let description ?arranger_links version =
  with_span_placeholder @@
    let bars = Model.Version.bars version in
    let structure = Model.Version.structure version in
    let key = Model.Version.key version in
    let shape = spf "%d-bar %s version in %s" bars structure (Music.key_to_pretty_string key) in
    let%lwt arranger_block =
      match%lwt Model.Version.arrangers version with
      | [] -> lwt_nil
      | arrangers ->
        let name_block = Person.names' ?links: arranger_links arrangers in
        lwt ([txt " arranged by "; name_block])
    in
    let disambiguation_block =
      match Model.Version.disambiguation version with
      | "" -> []
      | disambiguation -> [txt (spf " (%s)" disambiguation)]
    in
    lwt ([txt shape] @ arranger_block @ disambiguation_block)

let description' ?arranger_links version =
  description ?arranger_links @@ Entry.value version

let name_gen version_gen =
  with_span_placeholder @@
    match version_gen with
    | Right (version, true) ->
      let%lwt name = Model.Version.name' version in
      lwt [a ~a: [a_href @@ Endpoints.Page.href_version @@ Entry.slug version] [txt name]]
    | Right (version, _) ->
      let%lwt name = Model.Version.name' version in
      lwt [txt name]
    | Left version ->
      let%lwt name = Model.Version.name version in
      lwt [txt name]

let name = name_gen % Either.left
let name' ?(link = true) version = name_gen @@ Right (version, link)

let name_and_dance_gen ?dance_link version parameters =
  with_span_placeholder @@
    let%lwt dance =
      match%lwt Model.VersionParameters.for_dance parameters with
      | None -> lwt_nil
      | Some dance ->
        lwt
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
    lwt (name_gen version :: dance)

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
  span (name_gen version :: disambiguation_block)

let name_and_disambiguation = name_and_disambiguation_gen % Either.left
let name_and_disambiguation' ?(name_link = true) version = name_and_disambiguation_gen @@ Right (version, name_link)

let name_disambiguation_and_sources' ?name_link version =
  (* FIXME: Use not books but actual sources. This should also avoid a search,
     and therefore allow splitting this formatter into a [_gen] one. *)
  let sources =
    with_span_placeholder @@
      let%lwt sources =
        snd
        <$> Madge_client.call_exn
            Endpoints.Api.(route @@ Book Search)
            Slice.everything
            Filter.Book.(Formula.and_ (memVersionDeep' version) isSource')
      in
      lwt @@
        match List.map Book.short_title' sources with
        | [] -> []
        | [title] -> [txt "Source: "; title]
        | titles ->
          titles
          |> List.interspersei (fun _ -> txt " - ")
          |> List.cons (txt "Sources: ")
  in
  span [
    name_and_disambiguation' ?name_link version;
    span ~a: [a_class ["opacity-50"]] [sources];
  ]

let disambiguation_and_sources' version =
  (* FIXME: same as above *)
  let sources =
    with_span_placeholder @@
      let%lwt sources =
        snd
        <$> Madge_client.call_exn
            Endpoints.Api.(route @@ Book Search)
            Slice.everything
            Filter.Book.(Formula.and_ (memVersionDeep' version) isSource')
      in
      lwt @@
        match List.map Book.short_title' sources with
        | [] -> []
        | [title] -> [txt "Source: "; title]
        | titles ->
          titles
          |> List.interspersei (fun _ -> txt " - ")
          |> List.cons (txt "Sources: ")
  in
  span
    [
      txt (Model.Version.disambiguation' version);
      span ~a: [a_class ["opacity-50"]] [sources];
    ]

let composer_and_arranger ?(short = false) ?arranger_links version =
  with_span_placeholder @@
    let%lwt composer_block = Tune.composers' ~short <$> Model.Version.tune version in
    let%lwt arranger_block =
      match%lwt Model.Version.arrangers version with
      | [] -> lwt_nil
      | arrangers ->
        let arr = if short then "arr." else "arranged by" in
        let arranger_block = Person.names' ~short ?links: arranger_links arrangers in
        lwt
          [
            span ~a: [a_class ["opacity-50"]] [txt (spf ", %s " arr); arranger_block]
          ]
    in
    lwt (composer_block :: arranger_block)

let composer_and_arranger' ?short ?arranger_links version =
  composer_and_arranger ?short ?arranger_links (Entry.value version)

let tune_aka version =
  with_span_placeholder
    (List.singleton <$> (Tune.aka' <$> Model.Version.tune version))

let tune_aka' = tune_aka % Entry.value

let tune_description version =
  with_span_placeholder
    (List.singleton <$> (Tune.description' <$> Model.Version.tune version))

let tune_description' = tune_description % Entry.value

let kind_and_structure version =
  with_span_placeholder @@
    let bars = Model.Version.bars version in
    let%lwt kind = Model.Tune.kind' <$> Model.Version.tune version in
    let structure = Model.Version.structure version in
    lwt [txt @@ Kind.Version.to_string (bars, kind) ^ " (" ^ structure ^ ")"]

let kind_and_structure' = kind_and_structure % Entry.value
