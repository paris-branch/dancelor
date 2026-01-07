open Nes
open Common

open Model
open Html

let make_source_result' ?classes ?action ?(prefix = []) ?(suffix = []) source =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell [Formatters.Source.name' ~link: false source];
      Result_row.cell [txt (Option.fold ~none: "" ~some: PartialDate.to_pretty_string (Source.date' source))];
      Result_row.lcell (List.singleton <$> (Formatters.Person.names' ~short: true <$> Source.editors' source));
      ] @
      suffix
    )

let make_source_result ?classes ?context ?prefix ?suffix source =
  make_source_result'
    ?classes
    ~action: (
      Result_row.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_source @@ Entry.id source)
          ~some: (S.map (fun context -> Endpoints.Page.href_source ~context @@ Entry.id source))
    )
    ?prefix
    ?suffix
    source

let make_person_result' ?classes ?action ?(prefix = []) ?(suffix = []) person =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell ~a: [a_colspan 3] [Formatters.Person.name' ~link: false person];
      ] @
      suffix
    )

let make_person_result ?classes ?context ?prefix ?suffix person =
  make_person_result'
    ?classes
    ~action: (
      Result_row.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_person @@ Entry.id person)
          ~some: (S.map (fun context -> Endpoints.Page.href_person ~context @@ Entry.id person))
    )
    ?prefix
    ?suffix
    person

let make_dance_result' ?classes ?action ?(prefix = []) ?(suffix = []) dance =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell [Formatters.Dance.name_and_disambiguation' ~name_link: false dance];
      Result_row.cell [txt (Kind.Dance.to_string @@ Dance.kind' dance)];
      Result_row.lcell (List.singleton <$> (Formatters.Person.names' ~short: true <$> Dance.devisers' dance));
      ] @
      suffix
    )

let make_dance_result ?classes ?context ?prefix ?suffix dance =
  make_dance_result'
    ?classes
    ~action: (
      Result_row.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_dance @@ Entry.id dance)
          ~some: (S.map (fun context -> Endpoints.Page.href_dance ~context @@ Entry.id dance))
    )
    ?prefix
    ?suffix
    dance

let make_book_result' ?classes ?action ?(prefix = []) ?(suffix = []) book =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell [Formatters.Book.title' ~link: false book];
      Result_row.cell [txt (Option.fold ~none: "" ~some: PartialDate.to_pretty_string (Book.date' book))];
      Result_row.cell [Formatters.Book.editors' book];
      ] @
      suffix
    )

let make_book_result ?classes ?context ?prefix ?suffix book =
  make_book_result'
    ?classes
    ~action: (
      Result_row.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_book @@ Entry.id book)
          ~some: (S.map (fun context -> Endpoints.Page.href_book ~context @@ Entry.id book))
    )
    ?prefix
    ?suffix
    book

let make_set_result' ?classes ?action ?(prefix = []) ?(suffix = []) set =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell [txt @@ NEString.to_string @@ Set.name' set];
      Result_row.cell [txt @@ Kind.Dance.to_string @@ Set.kind' set];
      Result_row.lcell (List.singleton <$> (Formatters.Person.names' ~short: true <$> Set.conceptors' set));
      ] @
      suffix
    )

let make_set_result ?classes ?context ?prefix ?suffix set =
  make_set_result'
    ?classes
    ~action: (
      Result_row.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_set @@ Entry.id set)
          ~some: (S.map (fun context -> Endpoints.Page.href_set ~context @@ Entry.id set))
    )
    ?prefix
    ?suffix
    set

let make_tune_result' ?classes ?action ?(prefix = []) ?(suffix = []) tune =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell [txt @@ NEString.to_string @@ Tune.one_name' tune];
      Result_row.cell [txt @@ Kind.Base.to_pretty_string ~capitalised: true @@ Tune.kind' tune];
      Result_row.cell [Formatters.Tune.composers' tune];
      ] @
      suffix
    )

let make_tune_result ?classes ?context ?prefix ?suffix tune =
  make_tune_result'
    ?classes
    ~action: (
      Result_row.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_tune @@ Entry.id tune)
          ~some: (S.map (fun context -> Endpoints.Page.href_tune ~context @@ Entry.id tune))
    )
    ?prefix
    ?suffix
    tune

let make_version_result' ?classes ?action ?(prefix = []) ?(suffix = []) version =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell [Formatters.Version.name_disambiguation_and_sources' ~name_link: false version];
      Result_row.cell [Formatters.Version.kind_and_structure' version];
      Result_row.cell [Formatters.Version.composer_and_arranger' ~short: true version];
      ] @
      suffix
    )

let make_version_result ?classes ?context ?prefix ?suffix version =
  make_version_result'
    ?classes
    ~action: (
      Result_row.link @@
        Option.fold
          context
          ~none: (S.const @@ Endpoints.Page.href_version @@ Entry.id version)
          ~some: (S.map (fun context -> Endpoints.Page.href_version ~context @@ Entry.id version))
    )
    ?prefix
    ?suffix
    version

let make_user_result' ?classes ?action ?(prefix = []) ?(suffix = []) user =
  Result_row.make
    ?classes
    ?action
    (
      prefix @
      [Result_row.cell ~a: [a_colspan 3] [txt @@ NEString.to_string @@ User.username' user];
      ] @
      suffix
    )

let make_user_result ?classes ?context ?prefix ?suffix user =
  ignore context;
  make_user_result'
    ?classes
    ~action: No_action
    (* FIXME: make a user href *)
    (* Result_row.link @@ *)
    (*   Option.fold *)
    (*     context *)
    (*     ~none: (S.const @@ Endpoints.Page.href_user @@ Entry.id user) *)
    (*     ~some: (S.map (fun context -> Endpoints.Page.href_user ~context @@ Entry.id user)) *)
    ?prefix
    ?suffix
    user

let any_type_to_icon any =
  Icon.Model (
    match (any : Any.Type.t) with
    | Source -> Source
    | Person -> Person
    | Dance -> Dance
    | Tune -> Tune
    | Version -> Version
    | Set -> Set
    | Book -> Book
    | User -> User
  )

let make_result ?classes ?context any =
  let type_ = Any.type_of any in
  let prefix = [
    Result_row.cell
      ~a: [a_class ["text-nowrap"]]
      [
        Icon.html (any_type_to_icon type_);
        span ~a: [a_class ["d-none"; "d-sm-inline"]] [txt " "; txt (Any.Type.to_string type_)];
      ];
  ]
  in
  let suffix =
    let tooltip_everyone =
      "You can see this entry because it is an always-public entry (eg. a person \
       or a tune), or because it was made public by its owner."
    in
    [
      Result_row.cell [
        Model.Any.to_entry'
          any
          ~on_public: (fun _entry ->
            span [Icon.html Icon.(Access Everyone) ~tooltip: tooltip_everyone]
          )
          ~on_private: (fun entry ->
            R.span @@
            S.from' [] @@
            let%lwt reason = Option.get <$> Permission.can_get_private entry in
            let (icon, tooltip) =
              match reason with
              | Everyone -> (Icon.(Access Everyone), tooltip_everyone)
              | Viewer -> (Icon.(Access Viewer), "You can see this entry because its owner marked you as one of its viewers.")
              | Owner -> (Icon.(Access Owner), "You can see this entry because you are (one of) its owners.")
              | Omniscient_administrator -> (Icon.(Access Omniscient_administrator), "You can see this entry because you are an administrator, with omniscience enabled. You would not be able to access it without that.")
            in
            lwt [Icon.html ~tooltip icon]
          )
      ]
    ]
  in
  match any with
  | Source source -> make_source_result ?classes ?context ~prefix ~suffix source
  | Person person -> make_person_result ?classes ?context ~prefix ~suffix person
  | Dance dance -> make_dance_result ?classes ?context ~prefix ~suffix dance
  | Book book -> make_book_result ?classes ?context ~prefix ~suffix book
  | Set set -> make_set_result ?classes ?context ~prefix ~suffix set
  | Tune tune -> make_tune_result ?classes ?context ~prefix ~suffix tune
  | Version version -> make_version_result ?classes ?context ~prefix ~suffix version
  | User user -> make_user_result ?classes ?context ~prefix ~suffix user
