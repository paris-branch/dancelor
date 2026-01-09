open Nes
open Common
open Model
open Html
open Utils

let clickable_row ?onclick cells =
  Result_row.(to_clickable_row @@ make ?onclick (List.map (fun content -> R.td (S.from' [] content)) cells))

let map_table ~header list fun_ =
  div
    ~a: [a_class ["table-responsive"]]
    [
      tablex
        ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-2"]]
        ~thead: (thead ~a: [a_class ["table-primary"]] [tr (List.map (fun str -> th [txt str]) header)])
        [tbody (List.map fun_ list)]
    ]

let books books =
  map_table ~header: ["Book"; "Date"] books @@ fun book ->
  clickable_row
    [
      lwt [Formatters.Book.title' book];
      lwt [txt @@ Option.fold ~none: "" ~some: PartialDate.to_pretty_string @@ Book.date' book]
    ]

let sets sets =
  map_table ~header: ["Name"; "Deviser"; "Kind"] sets @@ fun set ->
  clickable_row
    [
      lwt [
        Formatters.Set.name' set;
        br ();
        small [Formatters.Set.tunes' set];
      ];
      (List.singleton <$> (Formatters.Person.names' <$> Set.conceptors' set));
      lwt [txt @@ Kind.Dance.to_string @@ Set.kind' set];
    ]

let dances dances =
  map_table ~header: ["Name"; "Deviser"; "Kind"] dances @@ fun dance ->
  clickable_row
    [
      lwt [Formatters.Dance.name_and_disambiguation' dance];
      (List.singleton <$> (Formatters.Person.names' <$> Dance.devisers' dance));
      lwt [txt @@ Kind.Dance.to_string @@ Dance.kind' dance];
    ]

let tunes tunes =
  map_table ~header: ["Name"; "Kind"; "Composer"] tunes @@ fun tune ->
  clickable_row
    [
      lwt [Formatters.Tune.name' tune];
      lwt [txt @@ Kind.Base.to_pretty_string ~capitalised: true @@ Tune.kind' tune];
      lwt [Formatters.Tune.composers' tune];
    ]

let versions ?onclick versions =
  map_table
    ~header: ["Disambiguation"; "Arranger"; "Kind"; "Key"; "Structure"]
    versions
    @@ fun version ->
    let onclick = Option.map (fun onclick -> fun () -> onclick version) onclick in
    clickable_row
      ?onclick
      [
        lwt [Formatters.Version.disambiguation_and_sources' ~parentheses: false version];
        (List.singleton <$> (Formatters.Person.names' <$> Version.arrangers' version));
        (List.singleton <$> (txt % Kind.Base.to_pretty_string % Tune.kind' <$> Version.tune' version));
        lwt [txt @@ Music.Key.to_pretty_string @@ Version.key' version];
        lwt [
          txt @@
            match Version.content' version with
            | Monolithic {structure; _} -> NEString.to_string @@ Version.Structure.to_string structure
            | Destructured _ -> "destr."
        ];
      ]

let versions_with_names versions =
  map_table
    ~header: ["Name"; "Kind"; "Key"; "Structure"]
    versions
    @@ fun version ->
    clickable_row
      [
        lwt [with_span_placeholder (List.singleton % txt % NEString.to_string % Tune.one_name' <$> Version.tune' version)];
        (List.singleton <$> (txt % Kind.Base.to_pretty_string % Tune.kind' <$> Version.tune' version));
        lwt [txt @@ Music.Key.to_pretty_string @@ Version.key' version];
        lwt [
          txt @@
            match Version.content' version with
            | Monolithic {structure; _} -> NEString.to_string @@ Version.Structure.to_string structure
            | Destructured _ -> "destr."
        ];
      ]

let placeholder ?(show_thead = true) ?(show_tfoot = true) () = [
  div
    ~a: [a_class ["table-responsive"]]
    [
      tablex
        ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-1"]]
        ?thead: (
          if show_thead then
            some @@
              thead
                ~a: [a_class ["table-primary"]]
                [
                  tr [
                    th [span_placeholder ()];
                    th [span_placeholder ()];
                    th [span_placeholder ()];
                  ];
                ]
          else None
        )
        ?tfoot: (
          if show_tfoot then
            some @@
              tfoot
                ~a: [a_class ["table-primary"]]
                [
                  tr [
                    th [span_placeholder ()];
                    th [span_placeholder ()];
                    th [span_placeholder ()];
                  ];
                ]
          else None
        )
        [
          tbody [
            tr [
              td [span_placeholder ()];
              td [span_placeholder ()];
              td [span_placeholder ()];
            ];
            tr [
              td [span_placeholder ()];
              td [span_placeholder ()];
              td [span_placeholder ()];
            ];
            tr [
              td [span_placeholder ()];
              td [span_placeholder ()];
              td [span_placeholder ()];
            ];
          ]
        ]
    ]
]
