open Nes
open Common

open Model
open Html

let make_source_result' ?classes ?action ?(prefix = []) ?(suffix = []) source =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [ResultRow.cell ~a: [a_colspan 2] [Formatters.Source.name' ~link: false source];
      ResultRow.lcell (List.singleton <$> (Formatters.Person.names' ~short: true <$> Source.editors' source));
      ] @
      suffix
    )

let make_source_result ?classes ?context ?prefix ?suffix source =
  make_source_result'
    ?classes
    ~action: (
      ResultRow.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_source @@ Entry.id source)
          ~some: (S.map (fun context -> Endpoints.Page.href_source ~context @@ Entry.id source))
    )
    ?prefix
    ?suffix
    source

let make_person_result' ?classes ?action ?(prefix = []) ?(suffix = []) person =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [ResultRow.cell ~a: [a_colspan 3] [Formatters.Person.name' ~link: false person];
      ] @
      suffix
    )

let make_person_result ?classes ?context ?prefix ?suffix person =
  make_person_result'
    ?classes
    ~action: (
      ResultRow.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_person @@ Entry.id person)
          ~some: (S.map (fun context -> Endpoints.Page.href_person ~context @@ Entry.id person))
    )
    ?prefix
    ?suffix
    person

let make_dance_result' ?classes ?action ?(prefix = []) ?(suffix = []) dance =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [ResultRow.cell [Formatters.Dance.name_and_disambiguation' ~name_link: false dance];
      ResultRow.cell [txt (Kind.Dance.to_string @@ Dance.kind' dance)];
      ResultRow.lcell (List.singleton <$> (Formatters.Person.names' ~short: true <$> Dance.devisers' dance));
      ] @
      suffix
    )

let make_dance_result ?classes ?context ?prefix ?suffix dance =
  make_dance_result'
    ?classes
    ~action: (
      ResultRow.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_dance @@ Entry.id dance)
          ~some: (S.map (fun context -> Endpoints.Page.href_dance ~context @@ Entry.id dance))
    )
    ?prefix
    ?suffix
    dance

let make_book_result' ?classes ?action ?(prefix = []) ?(suffix = []) book =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [ResultRow.cell [Formatters.Book.title_and_subtitle' book];
      ResultRow.cell ~a: [a_colspan 2] [txt (Option.fold ~none: "" ~some: PartialDate.to_pretty_string (Book.date' book))];
      ] @
      suffix
    )

let make_book_result ?classes ?context ?prefix ?suffix book =
  make_book_result'
    ?classes
    ~action: (
      ResultRow.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_book @@ Entry.id book)
          ~some: (S.map (fun context -> Endpoints.Page.href_book ~context @@ Entry.id book))
    )
    ?prefix
    ?suffix
    book

let make_set_result' ?classes ?action ?(prefix = []) ?(suffix = []) set =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [ResultRow.cell [txt @@ Set.name' set];
      ResultRow.cell [txt @@ Kind.Dance.to_string @@ Set.kind' set];
      ResultRow.lcell (List.singleton <$> (Formatters.Person.names' ~short: true <$> Set.conceptors' set));
      ] @
      suffix
    )

let make_set_result ?classes ?context ?prefix ?suffix set =
  make_set_result'
    ?classes
    ~action: (
      ResultRow.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_set @@ Entry.id set)
          ~some: (S.map (fun context -> Endpoints.Page.href_set ~context @@ Entry.id set))
    )
    ?prefix
    ?suffix
    set

let make_tune_result' ?classes ?action ?(prefix = []) ?(suffix = []) tune =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [ResultRow.cell [txt @@ Tune.name' tune];
      ResultRow.cell [txt @@ Kind.Base.to_pretty_string ~capitalised: true @@ Tune.kind' tune];
      ResultRow.cell [Formatters.Tune.composers' tune];
      ] @
      suffix
    )

let make_tune_result ?classes ?context ?prefix ?suffix tune =
  make_tune_result'
    ?classes
    ~action: (
      ResultRow.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_tune @@ Entry.id tune)
          ~some: (S.map (fun context -> Endpoints.Page.href_tune ~context @@ Entry.id tune))
    )
    ?prefix
    ?suffix
    tune

let make_version_result' ?classes ?action ?(prefix = []) ?(suffix = []) version =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [ResultRow.cell [Formatters.Version.name_disambiguation_and_sources' ~name_link: false version];
      ResultRow.cell [Formatters.Version.kind_and_structure' version];
      ResultRow.cell [Formatters.Version.composer_and_arranger' ~short: true version];
      ] @
      suffix
    )

let make_version_result ?classes ?context ?prefix ?suffix version =
  make_version_result'
    ?classes
    ~action: (
      ResultRow.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_version @@ Entry.id version)
          ~some: (S.map (fun context -> Endpoints.Page.href_version ~context @@ Entry.id version))
    )
    ?prefix
    ?suffix
    version

let any_type_to_bi = function
  | Any.Type.Source -> "bi-archive"
  | Person -> "bi-person"
  | Dance -> "bi-person-arms-up"
  | Tune -> "bi-music-note-list"
  | Version -> "bi-music-note-beamed"
  | Set -> "bi-list-stars"
  | Book -> "bi-book"

let make_result ?classes ?context any =
  let type_ = Any.type_of any in
  let prefix = [
    ResultRow.cell
      ~a: [a_class ["text-nowrap"]]
      [
        i ~a: [a_class ["bi"; any_type_to_bi type_]] [];
        span ~a: [a_class ["d-none"; "d-sm-inline"]] [txt " "; txt (Any.Type.to_string type_)];
      ];
  ]
  in
  match any with
  | Source source -> make_source_result ?classes ?context ~prefix source
  | Person person -> make_person_result ?classes ?context ~prefix person
  | Dance dance -> make_dance_result ?classes ?context ~prefix dance
  | Book book -> make_book_result ?classes ?context ~prefix book
  | Set set -> make_set_result ?classes ?context ~prefix set
  | Tune tune -> make_tune_result ?classes ?context ~prefix tune
  | Version version -> make_version_result ?classes ?context ~prefix version
