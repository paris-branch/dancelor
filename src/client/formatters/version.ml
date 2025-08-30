open Nes
open Common

open Html

let disambiguation_and_sources_internal ?source_links version =
  let disambiguation_block =
    match Model.Version.disambiguation version with
    | "" -> []
    | disambiguation -> [txt (spf " (%s)" disambiguation)]
  in
  let sources_block =
    match%lwt Model.Version.sources version with
    | [] -> lwt_nil
    | sources ->
      lwt (
        [txt " (from "] @
        List.interspersei (fun _ -> txt " - ") (List.map (Source.name' ~short: true ?link: source_links) sources) @
          [txt ")"]
      )
  in
  disambiguation_block @ [with_span_placeholder sources_block]

let disambiguation_and_sources ?source_links version = span (disambiguation_and_sources_internal ?source_links version)
let disambiguation_and_sources' ?source_links version = disambiguation_and_sources ?source_links @@ Entry.value version

let description ?arranger_links ?source_links version =
  let bars = Model.Version.bars version in
  let structure = Model.Version.structure version in
  let key = Model.Version.key version in
  let shape = spf "%d-bar %s version in %s" bars structure (Music.key_to_pretty_string key) in
  let arranger_block =
    match%lwt Model.Version.arrangers version with
    | [] -> lwt_nil
    | arrangers ->
      let name_block = Person.names' ?links: arranger_links arrangers in
      lwt ([txt " arranged by "; name_block])
  in
  span [txt shape; with_span_placeholder arranger_block; disambiguation_and_sources ?source_links version]

let description' ?arranger_links ?source_links version =
  description ?arranger_links ?source_links @@ Entry.value version

let name_gen version_gen =
  with_span_placeholder @@
    match version_gen with
    | Right (version, true, context) ->
      let%lwt name = Model.Version.one_name' version in
      lwt [a ~a: [a_href @@ Endpoints.Page.href_version ?context @@ Entry.id version] [txt @@ NEString.to_string name]]
    | Right (version, _, _) ->
      let%lwt name = Model.Version.one_name' version in
      lwt [txt @@ NEString.to_string name]
    | Left version ->
      let%lwt name = Model.Version.one_name version in
      lwt [txt @@ NEString.to_string name]

let name = name_gen % Either.left
let name' ?(link = true) ?context version = name_gen @@ Right (version, link, context)

let name_disambiguation_and_sources_gen ?(params = Model.VersionParameters.none) version =
  let disambiguation_and_sources_block =
    let version =
      match version with
      | Left version -> version
      | Right (version, _, _) -> Entry.value version
    in
    disambiguation_and_sources_internal version
  in
  let display_name_block =
    match Model.VersionParameters.display_name params with
    | None -> []
    | Some display_name -> [txt " [as “"; txt (NEString.to_string display_name); txt "”]"]
  in
  span (
    [name_gen version; span ~a: [a_class ["opacity-50"]] disambiguation_and_sources_block] @
      display_name_block
  )

let name_disambiguation_and_sources ?params version =
  name_disambiguation_and_sources_gen ?params (Either.left version)

let name_disambiguation_and_sources' ?(name_link = true) ?context ?params version =
  name_disambiguation_and_sources_gen ?params @@ Right (version, name_link, context)

let composer_and_arranger ?(short = false) ?arranger_links ?(params = Model.VersionParameters.none) version =
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
    let display_composer_block =
      match Model.VersionParameters.display_composer params with
      | None -> []
      | Some display_composer -> [txt " [as “"; txt (NEString.to_string display_composer); txt "”]"]
    in
    lwt ([composer_block] @ arranger_block @ display_composer_block)

let composer_and_arranger' ?short ?arranger_links ?params version =
  composer_and_arranger ?short ?arranger_links ?params (Entry.value version)

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
