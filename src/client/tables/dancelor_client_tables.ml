open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
module Formatters = Dancelor_client_formatters

let clickable_row ?href ?href_lwt cells =
  tr ~classes:["clickable"] (
    List.map
      (fun cell_lwt ->
         td [ a_lwt ~classes:["fill"] ?href ?href_lwt cell_lwt ])
      cells
  )

let map_table ~header list fun_ =
  table ~classes:["separated-table"; "visible"] [
    thead [ tr (List.map (fun str -> th [ text str ]) header) ];
    tbody (List.map fun_ list)
  ]

let books books =
  map_table ~header:[ "Book"; "Date" ] books @@ fun book ->
  let href_lwt =
    let%lwt slug = Book.slug book in
    Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
  in
  clickable_row ~href_lwt [
    Formatters.Book.title_and_subtitle book;
    Lwt.return [ text_lwt (let open Lwt in Book.date book >|= NesDate.to_string) ]
  ]

let sets sets =
  map_table ~header:[ "Name"; "Deviser"; "Kind" ] sets @@ fun set ->
  let href_lwt =
    let%lwt slug = Set.slug set in
    Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
  in
  clickable_row ~href_lwt [
    (Formatters.Set.name_and_tunes set);
    (Set.deviser set >>=| Formatters.Credit.line);
    Lwt.return [ text_lwt (Set.kind set >|=| Kind.dance_to_string) ];
  ]

let dances dances =
  map_table ~header:[ "Name"; "Deviser"; "Kind" ] dances @@ fun dance ->
  let href_lwt =
    let%lwt slug = Dance.slug dance in
    Lwt.return (Router.path_of_controller (Router.Dance slug) |> snd)
  in
  clickable_row ~href_lwt [
    Lwt.return [ text_lwt (Dance.name dance) ];
    (Dance.deviser dance >>=| Formatters.Credit.line);
    Lwt.return [ text_lwt (Dance.kind dance >|=| Kind.dance_to_string) ];
  ]

let tunes tunes =
  map_table ~header:[ "Name"; "Kind"; "Author" ] tunes @@ fun tune ->
  let href_lwt =
    let%lwt slug = Tune.slug tune in
    Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
  in
  clickable_row ~href_lwt [
    Lwt.return [ text_lwt (Tune.name tune) ];
    Lwt.return [ text_lwt (Tune.kind tune >|=| Kind.base_to_pretty_string ~capitalised:true) ];
    (Tune.author tune >>=| Formatters.Credit.line);
  ]

let versions versions =
  map_table ~header:[ "Disambiguation"; "Arranger"; "Kind"; "Key"; "Structure" ]
    versions @@ fun version ->
  let tune_lwt = Version.tune version in
  let href_lwt =
    let%lwt slug = Version.slug version in
    Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
  in
  clickable_row ~href_lwt [
    (Formatters.Version.disambiguation_and_sources version);
    (Version.arranger version >>=| Formatters.Credit.line);
    (tune_lwt >>=| Formatters.Kind.full_string version);
    Lwt.return [ text_lwt (Version.key version >|=| Music.key_to_pretty_string) ];
    Lwt.return [ text_lwt (Version.structure version) ];
  ]
