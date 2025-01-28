open Nes
open Model
open Html
module PageRouter = Dancelor_common.PageRouter
module Database = Dancelor_common.Database

let clickable_row ~href cells =
  Utils.ResultRow.(to_clickable_row @@ make ~href (List.map lcell cells))

let map_table ~header list fun_ =
  tablex
    ~a: [a_class ["separated-table"; "visible"]]
    ~thead: (thead [tr (List.map (fun str -> th [txt str]) header)])
    [tbody (List.map fun_ list)]

let books books =
  map_table ~header: ["Book"; "Date"] books @@ fun book ->
  clickable_row
    ~href: (PageRouter.href_book @@ Database.Entry.slug book)
    [
      (Lwt.return @@ Formatters.Book.title_and_subtitle book);
      Lwt.return [txt @@ Option.fold ~none: "" ~some: PartialDate.to_pretty_string @@ Book.date book]
    ]

let sets sets =
  map_table ~header: ["Name"; "Deviser"; "Kind"] sets @@ fun set ->
  let href = PageRouter.href_set @@ Database.Entry.slug set in
  clickable_row
    ~href
    [
      (Formatters.Set.name_and_tunes ~link: false set);
      (Lwt.map Formatters.Person.names (Set.conceptors set));
      Lwt.return [txt @@ Dancelor_common.Kind.Dance.to_string @@ Set.kind set];
    ]

let dances dances =
  map_table ~header: ["Name"; "Deviser"; "Kind"] dances @@ fun dance ->
  let href = PageRouter.href_dance @@ Database.Entry.slug dance in
  clickable_row
    ~href
    [
      (Lwt.return @@ Formatters.Dance.name ~link: false dance);
      (Lwt.map Formatters.Person.names (Dance.devisers dance));
      Lwt.return [txt @@ Dancelor_common.Kind.Dance.to_string @@ Dance.kind dance];
    ]

let tunes tunes =
  map_table ~header: ["Name"; "Kind"; "Composer"] tunes @@ fun tune ->
  let href = PageRouter.href_tune @@ Database.Entry.slug tune in
  clickable_row
    ~href
    [
      (Lwt.return @@ Formatters.Tune.name ~link: false tune);
      Lwt.return [txt @@ Dancelor_common.Kind.Base.to_pretty_string ~capitalised: true @@ Tune.kind tune];
      (Formatters.Tune.composers tune);
    ]

let versions versions =
  map_table
    ~header: ["Disambiguation"; "Arranger"; "Kind"; "Key"; "Structure"]
    versions
  @@ fun version ->
  let tune_lwt = Version.tune version in
  let href = PageRouter.href_version @@ Database.Entry.slug version in
  clickable_row
    ~href
    [
      (Formatters.Version.disambiguation_and_sources version);
      (Lwt.map Formatters.Person.names (Version.arrangers version));
      (tune_lwt >>=| Formatters.Kind.full_string version);
      Lwt.return [txt @@ Dancelor_common.Music.key_to_pretty_string @@ Version.key version];
      Lwt.return [txt @@ Version.structure version];
    ]

let versions_with_names versions =
  map_table
    ~header: ["Name"; "Kind"; "Key"; "Structure"]
    versions
  @@ fun version ->
  let tune_lwt = Version.tune version in
  let href = PageRouter.href_version @@ Database.Entry.slug version in
  clickable_row
    ~href
    [
      Lwt.return [L.txt @@ Lwt.map Tune.name tune_lwt];
      (tune_lwt >>=| Formatters.Kind.full_string version);
      Lwt.return [txt @@ Dancelor_common.Music.key_to_pretty_string @@ Version.key version];
      Lwt.return [txt @@ Version.structure version];
    ]
