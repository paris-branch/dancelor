open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
module Formatters = Dancelor_client_formatters

let clickable_row ?href ?href_lwt cells =
  tr ~classes:["clickable"] const (
    List.map
      (fun cell_lwt ->
         td const [a ~classes:["fill"] ?href ?href_lwt lwt cell_lwt])
      cells
  )

let map_table ~header list fun_ =
  table ~classes:["separated-table"; "visible"] const [
    thead const [tr const (List.map (fun str -> th const [text const str]) header) ];
    tbody const (List.map fun_ list)
  ]

let books books =
  map_table ~header:[ "Book"; "Date" ] books @@ fun book ->
  let href_lwt =
    let%lwt slug = Book.slug book in
    Lwt.return PageRouter.(path (Book slug))
  in
  clickable_row ~href_lwt [
    Formatters.Book.title_and_subtitle book;
    Lwt.return [text lwt (
        let open Lwt in
        Book.date book >|= function
        | None -> ""
        | Some date -> NesPartialDate.to_pretty_string date
      )]
  ]

let sets sets =
  map_table ~header:[ "Name"; "Deviser"; "Kind" ] sets @@ fun set ->
  let href_lwt =
    let%lwt slug = Set.slug set in
    Lwt.return PageRouter.(path (Set slug))
  in
  clickable_row ~href_lwt [
    (Formatters.Set.name_and_tunes ~link:false set);
    (Set.deviser set >>=| Formatters.Credit.line);
    Lwt.return [text lwt (Set.kind set >|=| Kind.Dance.to_string)];
  ]

let dances dances =
  map_table ~header:[ "Name"; "Deviser"; "Kind" ] dances @@ fun dance ->
  let href_lwt =
    let%lwt slug = Dance.slug dance in
    Lwt.return PageRouter.(path (Dance slug))
  in
  clickable_row ~href_lwt [
    Lwt.return [text lwt (Dance.name dance)];
    (Dance.deviser dance >>=| Formatters.Credit.line);
    Lwt.return [text lwt (Dance.kind dance >|=| Kind.Dance.to_string)];
  ]

let tunes tunes =
  map_table ~header:[ "Name"; "Kind"; "Author" ] tunes @@ fun tune ->
  let href_lwt =
    let%lwt slug = Tune.slug tune in
    Lwt.return PageRouter.(path (Tune slug))
  in
  clickable_row ~href_lwt [
    Lwt.return [text lwt (Tune.name tune)];
    Lwt.return [text lwt (Tune.kind tune >|=| Kind.Base.to_pretty_string ~capitalised:true)];
    (Tune.author tune >>=| Formatters.Credit.line);
  ]

let versions versions =
  map_table ~header:[ "Disambiguation"; "Arranger"; "Kind"; "Key"; "Structure" ]
    versions @@ fun version ->
  let tune_lwt = Version.tune version in
  let href_lwt =
    let%lwt slug = Version.slug version in
    Lwt.return PageRouter.(path (Version slug))
  in
  clickable_row ~href_lwt [
    (Formatters.Version.disambiguation_and_sources version);
    (Version.arranger version >>=| Formatters.Credit.line);
    (tune_lwt >>=| Formatters.Kind.full_string version);
    Lwt.return [text lwt (Version.key version >|=| Music.key_to_pretty_string)];
    Lwt.return [text lwt (Version.structure version)];
  ]

let versions_with_names versions =
  map_table ~header:[ "Name"; "Kind"; "Key"; "Structure" ]
    versions @@ fun version ->
  let tune_lwt = Version.tune version in
  let href_lwt =
    let%lwt slug = Version.slug version in
    Lwt.return PageRouter.(path (Version slug))
  in
  clickable_row ~href_lwt [
    Lwt.return [text lwt (tune_lwt >>=| Tune.name)];
    (tune_lwt >>=| Formatters.Kind.full_string version);
    Lwt.return [text lwt (Version.key version >|=| Music.key_to_pretty_string)];
    Lwt.return [text lwt (Version.structure version)];
  ]

