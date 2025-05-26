open Nes
open Common

open Model
open Html

let clickable_row ~href cells =
  Utils.ResultRow.(to_clickable_row @@ make ~href (List.map lcell cells))

let map_table ~header list fun_ =
  tablex
    ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-2"]]
    ~thead: (thead ~a: [a_class ["table-primary"]] [tr (List.map (fun str -> th [txt str]) header)])
    [tbody (List.map fun_ list)]

let books books =
  map_table ~header: ["Book"; "Date"] books @@ fun book ->
  clickable_row
    ~href: (Endpoints.Page.href_book @@ Entry.slug book)
    [
      (Lwt.return @@ Formatters.Book.title_and_subtitle' book);
      Lwt.return [txt @@ Option.fold ~none: "" ~some: PartialDate.to_pretty_string @@ Book.date' book]
    ]

let sets sets =
  map_table ~header: ["Name"; "Deviser"; "Kind"] sets @@ fun set ->
  let href = Endpoints.Page.href_set @@ Entry.slug set in
  clickable_row
    ~href
    [
      (Formatters.Set.name_and_tunes' ~name_link: false set);
      (Lwt.map Formatters.Person.names' (Set.conceptors' set));
      Lwt.return [txt @@ Kind.Dance.to_string @@ Set.kind' set];
    ]

let dances dances =
  map_table ~header: ["Name"; "Deviser"; "Kind"] dances @@ fun dance ->
  let href = Endpoints.Page.href_dance @@ Entry.slug dance in
  clickable_row
    ~href
    [
      (Formatters.Dance.name_and_disambiguation' ~name_link: false dance);
      (Lwt.map Formatters.Person.names' (Dance.devisers' dance));
      Lwt.return [txt @@ Kind.Dance.to_string @@ Dance.kind' dance];
    ]

let tunes tunes =
  map_table ~header: ["Name"; "Kind"; "Composer"] tunes @@ fun tune ->
  let href = Endpoints.Page.href_tune @@ Entry.slug tune in
  clickable_row
    ~href
    [
      Lwt.return [Formatters.Tune.name' ~link: false tune];
      Lwt.return [txt @@ Kind.Base.to_pretty_string ~capitalised: true @@ Tune.kind' tune];
      (Formatters.Tune.composers' tune);
    ]

let versions versions =
  map_table
    ~header: ["Disambiguation"; "Arranger"; "Kind"; "Key"; "Structure"]
    versions
    @@ fun version ->
    let tune_lwt = Version.tune' version in
    let href = Endpoints.Page.href_version @@ Entry.slug version in
    clickable_row
      ~href
      [
        Lwt.return [Formatters.Version.disambiguation_and_sources' version];
        (Lwt.map Formatters.Person.names' (Version.arrangers' version));
        (tune_lwt >>=| Formatters.Kind.full_string version);
        Lwt.return [txt @@ Music.key_to_pretty_string @@ Version.key' version];
        Lwt.return [txt @@ Version.structure' version];
      ]

let versions_with_names versions =
  map_table
    ~header: ["Name"; "Kind"; "Key"; "Structure"]
    versions
    @@ fun version ->
    let tune_lwt = Version.tune' version in
    let href = Endpoints.Page.href_version @@ Entry.slug version in
    clickable_row
      ~href
      [
        Lwt.return [L.txt @@ Lwt.map Tune.name' tune_lwt];
        (tune_lwt >>=| Formatters.Kind.full_string version);
        Lwt.return [txt @@ Music.key_to_pretty_string @@ Version.key' version];
        Lwt.return [txt @@ Version.structure' version];
      ]
