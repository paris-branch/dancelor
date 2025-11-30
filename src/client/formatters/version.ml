open Nes
open Common

open Html

let disambiguation ?(parentheses = true) version =
  span @@
    match Model.Version.disambiguation version with
    | "" -> []
    | disambiguation when parentheses -> [txt (spf " (%s)" disambiguation)]
    | disambiguation -> [txt (spf " %s" disambiguation)]

let disambiguation' ?parentheses version =
  disambiguation ?parentheses (Entry.value version)

let disambiguation_and_sources_internal ?(parentheses = true) ?source_links version =
  let sources_block =
    match%lwt Model.Version.sources version with
    | [] -> lwt_nil
    | sources ->
      lwt @@
      List.flatten @@
      List.flatten
        [
          [[txt (if parentheses then " (from " else " from ")]];
          List.interspersei
            (fun _ -> [txt ", "])
            ~last: (fun _ -> [txt " and "])
            (
              List.map
                (fun {Model.Version.source; details; _} ->
                  [
                    Source.name' ~short: true ?link: source_links source;
                    (if details <> "" then txtf " %s" details else txt "");
                  ]
                )
                sources
            );
          [[txt (if parentheses then ")" else "")]];
        ]
  in
    [with_span_placeholder sources_block; disambiguation ~parentheses version]

let disambiguation_and_sources ?parentheses ?source_links version =
  span (disambiguation_and_sources_internal ?parentheses ?source_links version)

let disambiguation_and_sources' ?parentheses ?source_links version =
  disambiguation_and_sources ?parentheses ?source_links @@ Entry.value version

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
  let the_version = match version with Right (version, _, _) -> Entry.value version | Left version -> version in
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
    | Some display_name -> [txtf " [as “%s”]" (NEString.to_string display_name)]
  in
  let structure_block =
    match Model.VersionParameters.structure params with
    | None -> []
    | Some structure -> [txtf " [play %s]" (NEString.to_string @@ Model.Version.Structure.to_string structure)]
  in
  let transposition_block =
    match Model.VersionParameters.transposition params with
    | None -> []
    | Some transposition ->
      let key =
        Music.Key.with_pitch (fun source -> Transposition.target_pitch ~source transposition) (Model.Version.key the_version)
      in
        [txtf " [in %s / %+d m2]" (Music.Key.to_pretty_string key) (Transposition.to_semitones transposition)]
  in
  span (
    [name_gen version; span ~a: [a_class ["opacity-50"]] disambiguation_and_sources_block] @
    display_name_block @ structure_block @ transposition_block
  )

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

let tune_description version =
  with_span_placeholder
    (List.singleton <$> (Tune.description' <$> Model.Version.tune version))

let tune_description' = tune_description % Entry.value

let kind_and_structure version =
  with_span_placeholder @@
    let%lwt kind = Model.Tune.kind' <$> Model.Version.tune version in
    match Model.Version.content version with
    | Monolithic {bars; structure; _} ->
      lwt [txt @@ Kind.Version.to_string (bars, kind) ^ " (" ^ NEString.to_string (Model.Version.Structure.to_string structure) ^ ")"]
    | Destructured _ ->
      lwt [txt @@ "∗ " ^ Kind.Base.to_string kind ^ " (destr.)"]

let kind_and_structure' = kind_and_structure % Entry.value

let id' version =
  span [a ~a: [a_href @@ Endpoints.Page.href_version @@ Entry.id version] [txt @@ Entry.id_as_string version]]

let several f versions =
  versions
  |> List.map (List.singleton % f)
  |> List.interspersei ~last: (fun _ -> [txt " and "]) (fun _ -> [txt ", "])
  |> List.flatten
  |> span

let names versions = several name @@ NEList.to_list versions
let names' ?links versions = several (name' ?link: links) @@ NEList.to_list versions

let composers_and_arrangers' ?short ?arranger_links versions = several (fun (version, params) -> composer_and_arranger' ?short ?arranger_links ~params version) @@ NEList.to_list versions

let names_disambiguations_and_sources' ?name_links versions = several (fun (version, params) -> name_disambiguation_and_sources' ?name_link: name_links ~params version) @@ NEList.to_list versions
