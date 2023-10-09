open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html.NewAPI
module Formatters = Dancelor_client_formatters

let clickable_row ~href cells =
  tr
    ~a:[
      a_class ["clickable"];
      a_onclick
        (fun _ ->
           let open Js_of_ocaml in
           Lwt.on_success href (fun href ->
               Dom_html.window##.location##.href := Js.string href);
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
  let href =
    let%lwt slug = Book.slug book in
    Lwt.return PageRouter.(path (Book slug))
  in
  clickable_row ~href [
    Formatters.BookNewAPI.title_and_subtitle book;
    Lwt.return [
      L.txt @@
      let open Lwt in
      Book.date book >|= function
      | None -> ""
      | Some date -> NesPartialDate.to_pretty_string date
    ]
  ]

let sets sets =
  map_table ~header: ["Name"; "Deviser"; "Kind"] sets @@ fun set ->
  let href =
    let%lwt slug = Set.slug set in
    Lwt.return PageRouter.(path (Set slug))
  in
  clickable_row ~href [
    (Formatters.SetNewAPI.name_and_tunes ~link:true set);
    (Set.deviser set >>=| Formatters.CreditNewAPI.line);
    Lwt.return [L.txt (Set.kind set >|=| Kind.Dance.to_string)];
  ]

let dances dances =
  map_table ~header:["Name"; "Deviser"; "Kind"] dances @@ fun dance ->
  let href =
    let%lwt slug = Dance.slug dance in
    Lwt.return PageRouter.(path (Dance slug))
  in
  clickable_row ~href [
    (Formatters.DanceNewAPI.name dance);
    (Dance.deviser dance >>=| Formatters.CreditNewAPI.line);
    Lwt.return [L.txt (Dance.kind dance >|=| Kind.Dance.to_string)];
  ]

let tunes tunes =
  map_table ~header:["Name"; "Kind"; "Author"] tunes @@ fun tune ->
  let href =
    let%lwt slug = Tune.slug tune in
    Lwt.return PageRouter.(path (Tune slug))
  in
  clickable_row ~href [
    (Formatters.TuneNewAPI.name tune);
    Lwt.return [L.txt (Tune.kind tune >|=| Kind.Base.to_pretty_string ~capitalised:true)];
    (Tune.author tune >>=| Formatters.CreditNewAPI.line);
  ]

let versions versions =
  map_table ~header:[ "Disambiguation"; "Arranger"; "Kind"; "Key"; "Structure" ]
    versions @@ fun version ->
  let tune_lwt = Version.tune version in
  let href =
    let%lwt slug = Version.slug version in
    Lwt.return PageRouter.(path (Version slug))
  in
  clickable_row ~href [
    (Formatters.VersionNewAPI.disambiguation_and_sources version);
    (Version.arranger version >>=| Formatters.CreditNewAPI.line);
    (tune_lwt >>=| Formatters.KindNewAPI.full_string version);
    Lwt.return [L.txt (Version.key version >|=| Music.key_to_pretty_string)];
    Lwt.return [L.txt (Version.structure version)];
  ]
