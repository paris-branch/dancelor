open Nes
open Common
open Model
open Html

let row ?(classes = []) ?onclick cells =
  let open Html in
  tr
    ~a: (
      List.filter_map id [
        Some (a_class classes);
        Option.map (fun f -> a_onclick (fun _ -> Lwt.async f; true)) onclick;
      ]
    )
    (cells)

let details content = p ~a: [a_class ["mb-0"; "opacity-50"; "lh-sm"]] [small content]

let make_part_result ?classes ?onclick ?(prefix = []) ?(suffix = []) title =
  row ?classes ?onclick (prefix @ [td ~a: [a_colspan 3] [txt @@ NEString.to_string title]] @ suffix)

let make_source_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) source =
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td (Lwt.pause ();%lwt lwt [Formatters.Source.name' ~link: (onclick = None) ?context source]);
      L.td (Lwt.pause ();%lwt lwt [txt (Option.fold ~none: "" ~some: (PartialDate.to_pretty_string ~short: true) (Source.date' source))]);
      L.td (Lwt.pause ();%lwt List.singleton <$> (Formatters.Person.names' ~short: true <$> Source.editors' source));
      ] @
      suffix
    )

let make_person_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) person =
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td ~a: [a_colspan 3] (Lwt.pause ();%lwt lwt [Formatters.Person.name' ~link: (onclick = None) ?context person]);
      ] @
      suffix
    )

let make_dance_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) dance =
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td (Lwt.pause ();%lwt lwt [Formatters.Dance.name_and_disambiguation' ~name_link: (onclick = None) ?context dance]);
      L.td (Lwt.pause ();%lwt lwt [txt (Kind.Dance.to_string @@ Dance.kind' dance)]);
      L.td (Lwt.pause ();%lwt List.singleton <$> (Formatters.Person.names' ~short: true <$> Dance.devisers' dance));
      ] @
      suffix
    )

let make_dance_plus_set_result ?classes ?onclick ?context ?set_params ?(prefix = []) ?(suffix = []) dance set =
  row ?classes ?onclick (
    prefix @
    [td [
      Formatters.Dance.name' ?context dance;
      details [txt "Set: "; Formatters.Set.name' ~link: true ?params: set_params set];
      details [Formatters.Set.tunes' ~link: true set];
    ];
    td [txt @@ Kind.Dance.to_string @@ Dance.kind' dance];
    td [Formatters.Set.conceptors' ~short: true ?params: set_params set];
    ] @
    suffix
  )

let make_dance_plus_versions_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) dance versions_and_params =
  row ?classes ?onclick (
    prefix @
    [td [
      Formatters.Dance.name' ?context dance;
      details [
        txt (if NEList.is_singleton versions_and_params then "Tune: " else "Tunes: ");
        Formatters.Version.names_disambiguations_and_sources' versions_and_params
      ];
    ];
    td [txt @@ Kind.Dance.to_string @@ Dance.kind' dance];
    td [Formatters.Version.composers_and_arrangers' ~short: true versions_and_params]] @
    suffix
  )

let make_book_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) book =
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td (Lwt.pause ();%lwt lwt [Formatters.Book.title' ~link: (onclick = None) ?context book]);
      L.td (Lwt.pause ();%lwt lwt [txt (Option.fold ~none: "" ~some: (PartialDate.to_pretty_string ~short: true) (Book.date' book))]);
      L.td (Lwt.pause ();%lwt lwt [Formatters.Book.editors' book]);
      ] @
      suffix
    )

let make_set_result ?classes ?onclick ?context ?params ?(prefix = []) ?(suffix = []) set =
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td (
        Lwt.pause ();%lwt
        lwt [
          Formatters.Set.name' ~link: (onclick = None) ?context ?params set;
          details [Formatters.Set.tunes' ~link: true set];
        ]
      );
      L.td (Lwt.pause ();%lwt lwt [txt @@ Kind.Dance.to_string @@ Set.kind' set]);
      L.td (Lwt.pause ();%lwt lwt [Formatters.Set.conceptors' ~short: true ?params set]);
      ] @
      suffix
    )

let make_tune_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) tune =
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td (Lwt.pause ();%lwt lwt [Formatters.Tune.name' ~link: (onclick = None) ?context tune]);
      L.td (Lwt.pause ();%lwt lwt [txt @@ Kind.Base.to_pretty_string ~capitalised: true @@ Tune.kind' tune]);
      L.td (Lwt.pause ();%lwt lwt [Formatters.Tune.composers' ~links: (onclick = None) tune]);
      ] @
      suffix
    )

let make_version_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) version =
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td (Lwt.pause ();%lwt lwt [Formatters.Version.name_disambiguation_and_sources' ~link: (onclick = None) ?context version]);
      L.td (Lwt.pause ();%lwt lwt [Formatters.Version.kind_and_structure' version]);
      L.td (Lwt.pause ();%lwt lwt [Formatters.Version.composer_and_arranger' ~link: (onclick = None) ~short: true version]);
      ] @
      suffix
    )

let make_versions_result ?classes ?onclick ?(prefix = []) ?(suffix = []) versions_and_params =
  row ?classes ?onclick (
    prefix @
    [td [Formatters.Version.names_disambiguations_and_sources' versions_and_params];
    (
      L.td (
        let%lwt all_kinds =
          List.sort_uniq Kind.Base.compare %
            NEList.to_list
          <$> NEList.map_lwt_p (Version.kind' % fst) versions_and_params
        in
        lwt [
          txt @@
            match all_kinds with
            | [kind] -> Kind.Base.to_string kind ^ (if NEList.is_singleton versions_and_params then "" else "s")
            | _ -> "Medley"
        ]
      )
    );
    td [Formatters.Version.composers_and_arrangers' ~short: true versions_and_params]] @
    suffix
  )

let make_user_result ?classes ?onclick ?context ?(prefix = []) ?(suffix = []) user =
  ignore context;
  row
    ?classes
    ?onclick
    (
      prefix @
      [L.td ~a: [a_colspan 3] (Lwt.pause ();%lwt lwt [txt @@ NEString.to_string @@ User.username' user]);
      ] @
      suffix
    )

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
  let prefix = [
    L.td
      ~a: [a_class ["text-nowrap"]]
      (
        Lwt.pause ();%lwt
        let type_ = Any.type_of any in
        lwt [
          Icon.html (any_type_to_icon type_);
          span ~a: [a_class ["d-none"; "d-sm-inline"]] [txt " "; txt (Any.Type.to_string type_)];
        ]
      );
  ]
  in
  let suffix = [
    L.td (
      Lwt.pause ();%lwt
      List.singleton
      <$> Model.Any.to_entry'
          any
          ~on_public: (fun _entry ->
            lwt (Icon.html Icon.(Access Everyone) ~tooltip: "You can see this entry because it is an always-public entry (eg. a person or a tune)" ~classes: ["opacity-25"])
          )
          ~on_private: (fun entry ->
            let%lwt reason = Option.get <$> Permission.can_get_private entry in
            let (icon, tooltip, classes) =
              match reason with
              | Everyone -> (Icon.(Access Everyone), "You can see this entry because it was made public by its owner.", ["opacity-50"])
              | Viewer -> (Icon.(Access Viewer), "You can see this entry because its owner marked you as one of its viewers.", ["opacity-75"])
              | Owner -> (Icon.(Access Owner), "You can see this entry because you are (one of) its owners.", [])
              | Omniscient_administrator -> (Icon.(Access Omniscient_administrator), "You can see this entry because you are an administrator, with omniscience enabled. You would not be able to access it without that.", [])
            in
            lwt (Icon.html icon ~tooltip ~classes)
          )
    )
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
