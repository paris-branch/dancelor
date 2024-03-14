open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
module Formatters = Dancelor_client_formatters

let clickable_row ~href cells =
  tr
    ~a:[
      a_class ["clickable"];
      a_onclick
        (fun _ ->
           let open Js_of_ocaml in
           Dom_html.window##.location##.href := Js.string href;
           true
        );
    ]
    (
      List.map L.td cells
    )

let map_table ~header list fun_ =
  tablex ~a:[a_class ["separated-table"; "visible"]]
    ~thead:(thead [tr (List.map (fun str -> th [txt str]) header)])
    [tbody (List.map fun_ list)]

let books books =
  map_table ~header:[ "Book"; "Date" ] books @@ fun book ->
  let href = PageRouter.path_book @@ Book.slug book in
  clickable_row ~href [
    (Lwt.return @@ Formatters.Book.title_and_subtitle book);
    Lwt.return [txt @@ Option.fold ~none:"" ~some:PartialDate.to_pretty_string @@ Book.date book]
  ]

let sets sets =
  map_table ~header: ["Name"; "Deviser"; "Kind"] sets @@ fun set ->
  let href = PageRouter.path_set @@ Set.slug set in
  clickable_row ~href [
    (Formatters.Set.name_and_tunes ~link:true set);
    (Lwt.map Formatters.Person.name (Set.deviser set));
    Lwt.return [txt @@ Kind.Dance.to_string @@ Set.kind set];
  ]

let dances dances =
  map_table ~header:["Name"; "Deviser"; "Kind"] dances @@ fun dance ->
  let href = PageRouter.path_dance @@ Dance.slug dance in
  clickable_row ~href [
    (Lwt.return @@ Formatters.Dance.name dance);
    (Lwt.map Formatters.Person.name (Dance.deviser dance));
    Lwt.return [txt @@ Kind.Dance.to_string @@ Dance.kind dance];
  ]

let tunes tunes =
  map_table ~header:["Name"; "Kind"; "Composer"] tunes @@ fun tune ->
  let href = PageRouter.path_tune @@ Tune.slug tune in
  clickable_row ~href [
    (Lwt.return @@ Formatters.Tune.name tune);
    Lwt.return [txt @@ Kind.Base.to_pretty_string ~capitalised:true @@ Tune.kind tune];
    (Lwt.map Formatters.Person.name (Tune.composer tune));
  ]

let versions versions =
  map_table ~header:[ "Disambiguation"; "Arranger"; "Kind"; "Key"; "Structure" ]
    versions @@ fun version ->
  let tune_lwt = Version.tune version in
  let href = PageRouter.path_version @@ Version.slug version in
  clickable_row ~href [
    (Formatters.Version.disambiguation_and_sources version);
    (Lwt.map Formatters.Person.name (Version.arranger version));
    (tune_lwt >>=| Formatters.Kind.full_string version);
    Lwt.return [txt @@ Music.key_to_pretty_string @@ Version.key version];
    Lwt.return [txt @@ Version.structure version];
  ]

let versions_with_names versions =
  map_table ~header:[ "Name"; "Kind"; "Key"; "Structure" ]
    versions @@ fun version ->
  let tune_lwt = Version.tune version in
  let href = PageRouter.path_version @@ Version.slug version in
  clickable_row ~href [
    Lwt.return [L.txt @@ Lwt.map Tune.name tune_lwt];
    (tune_lwt >>=| Formatters.Kind.full_string version);
    Lwt.return [txt @@ Music.key_to_pretty_string @@ Version.key version];
    Lwt.return [txt @@ Version.structure version];
  ]
