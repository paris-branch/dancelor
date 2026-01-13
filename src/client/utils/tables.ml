open Nes
open Html

let map_table ~header f list =
  div
    ~a: [a_class ["table-responsive"]]
    [
      tablex
        ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-2"]]
        ~thead: (thead ~a: [a_class ["table-primary"]] [tr (List.map (fun str -> th [txt str]) header)])
        [tbody (List.map f list)]
    ]

let dances dances =
  map_table ~header: ["Name"; "Kind"; "Deviser"] Any_result.make_dance_result dances

let tunes tunes =
  map_table ~header: ["Name"; "Kind"; "Composer"] Any_result.make_tune_result tunes

let versions ?onclick versions =
  map_table
    ~header: ["Name"; "Kind"; "Composer"]
    (fun version ->
      Any_result.make_version_result
        ?onclick: (Option.map (fun onclick () -> onclick version) onclick)
        version
    )
    versions

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
