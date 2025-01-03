open Nes
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
open Dancelor_client_html

let make_person_result' ?classes ?action ?(prefix = []) ?(suffix = []) person =
  ResultRow.make
    ?classes
    ?action
    (
      prefix @
      [
        ResultRow.cell ~a: [a_colspan 3] (Formatters.Person.name ~link: false person);
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
        ~none: (S.const @@ PageRouter.path_person @@ Person.slug person)
        ~some: (S.map (fun context -> PageRouter.path_person ~context @@ Person.slug person))
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
      [
        ResultRow.cell [txt (Dance.name dance)];
        ResultRow.cell [txt (Kind.Dance.to_string @@ Dance.kind dance)];
        ResultRow.lcell (Lwt.map (Formatters.Person.names ~short: true) (Dance.devisers dance));
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
        ~none: (S.const @@ PageRouter.path_dance @@ Dance.slug dance)
        ~some: (S.map (fun context -> PageRouter.path_dance ~context @@ Dance.slug dance))
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
      [
        ResultRow.cell (Formatters.Book.title_and_subtitle book);
        ResultRow.cell ~a: [a_colspan 2] [txt (Option.fold ~none: "" ~some: PartialDate.to_pretty_string (Book.date book))];
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
        ~none: (S.const @@ PageRouter.path_book @@ Book.slug book)
        ~some: (S.map (fun context -> PageRouter.path_book ~context @@ Book.slug book))
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
      [
        ResultRow.cell [txt @@ Set.name set];
        ResultRow.cell [txt @@ Kind.Dance.to_string @@ Set.kind set];
        ResultRow.lcell (Lwt.map (Formatters.Person.names ~short: true) (Set.conceptors set));
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
        ~none: (S.const @@ PageRouter.path_set @@ Set.slug set)
        ~some: (S.map (fun context -> PageRouter.path_set ~context @@ Set.slug set))
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
      [
        ResultRow.cell [txt @@ Tune.name tune];
        ResultRow.cell [txt @@ Kind.Base.to_pretty_string ~capitalised: true @@ Tune.kind tune];
        ResultRow.lcell (Formatters.Tune.composers tune);
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
        ~none: (S.const @@ PageRouter.path_tune @@ Tune.slug tune)
        ~some: (S.map (fun context -> PageRouter.path_tune ~context @@ Tune.slug tune))
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
      [
        ResultRow.lcell (Formatters.Version.name_and_disambiguation ~link: false version);
        ResultRow.cell
          [
            L.txt
              (
                let bars = Version.bars version in
                let%lwt kind = Lwt.map Tune.kind @@ Version.tune version in
                let structure = Version.structure version in
                Lwt.return (Kind.Version.to_string (bars, kind) ^ " (" ^ structure ^ ")")
              )
          ];
        ResultRow.lcell (Formatters.Version.composer_and_arranger ~short: true version);
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
        ~none: (S.const @@ PageRouter.path_version @@ Version.slug version)
        ~some: (S.map (fun context -> PageRouter.path_version ~context @@ Version.slug version))
    )
    ?prefix
    ?suffix
    version

let any_type_to_fa = function
  | Any.Type.Person -> "person"
  | Dance -> "directions_walk"
  | Tune -> "music_note"
  | Version -> "music_note"
  | Set -> "format_list_bulleted"
  | Book -> "library_books"

let make_result ?classes ?context any =
  let type_ = Any.type_of any in
  let prefix = [
    ResultRow.cell
      [
        i ~a: [a_class ["material-symbols-outlined"]] [txt @@ any_type_to_fa type_];
        txt " ";
        txt (Any.Type.to_string type_);
      ];
  ]
  in
  match any with
  | Person person -> make_person_result ?classes ?context ~prefix person
  | Dance dance -> make_dance_result ?classes ?context ~prefix dance
  | Book book -> make_book_result ?classes ?context ~prefix book
  | Set set -> make_set_result ?classes ?context ~prefix set
  | Tune tune -> make_tune_result ?classes ?context ~prefix tune
  | Version version -> make_version_result ?classes ?context ~prefix version
