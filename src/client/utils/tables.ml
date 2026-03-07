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

let any ?context anys =
  tablex
    ~a: [a_class ["table"; "table-striped"; "table-hover"; "table-borderless"; "my-1"]]
    ~thead: (
      thead
        ~a: [a_class ["table-primary"; "pe-none"]]
        [
          tr [
            th [span ~a: [a_class ["d-none"; "d-sm-inline"]] [txt "Type"]];
            th [txt "Name"];
            th [txt "Kind/date"];
            th [txt "By"];
            th []
          ]
        ]
    )
    ~tfoot: (
      tfoot
        ~a: [a_class ["table-primary"; "pe-none"]]
        [tr [td []; td []; td []; td []; td []]]
    )
    [tbody (List.map (Any_result.make_result ?context) anys)]

let placeholder ?(show_thead = true) ?(show_tfoot = true) ?(rows = 3) () = [
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
          tbody (
            List.init rows (fun _ ->
              tr [
                td [span_placeholder ()];
                td [span_placeholder ()];
                td [span_placeholder ()];
              ];
            )
          )
        ]
    ]
]
