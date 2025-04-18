open Js_of_ocaml
open Html

let make_content
    ?(label = "")
    ?label_processing
    ?icon
    ?badge
    ?(processing = S.const false)
    ()
  =
  List.concat @@
    List.filter_map
      Fun.id
      [
        Some
          [
            span
              ~a: [
                R.a_class
                  (
                    Fun.flip S.map processing @@ function
                      | true -> ["spinner-border"; "spinner-border-sm"; "me-2"]
                      | false -> ["d-none"]
                  );
                a_aria "hidden" ["true"]
              ]
              [];
            span
              ~a: [R.a_class (Fun.flip S.map processing @@ function true -> [] | false -> ["d-none"])]
              [txt @@ if label <> "" then " " else ""];
          ];
        (
          Fun.flip Option.map icon @@ fun icon ->
          [
            i
              ~a: [
                R.a_class
                  (
                    Fun.flip S.map processing @@ function
                      | true -> ["d-none"]
                      | false -> ["bi"; "bi-" ^ icon; "me-2"]
                  )
              ]
              [];
          ]
        );
        Some
          [
            R.txt
              (
                Fun.flip S.map processing @@ function
                  | true -> Option.value ~default: label label_processing
                  | false -> label
              );
          ];
        (
          Fun.flip Option.map badge @@ fun badge ->
          [
            span ~a: [a_class ["badge"; "text-bg-secondary"; "ms-2"]] [txt badge];
          ]
        )
      ]

let make
    ?label
    ?label_processing
    ?icon
    ?badge
    ?classes
    ?(disabled = S.const false)
    ~onclick
    ()
  =
  let (processing, set_processing) = React.S.create false in
  button
    ~a: [
      a_button_type `Button;
      R.a_class
        (
          let classes = "btn" :: Option.value ~default: [] classes in
          Fun.flip S.map (S.l2 (||) disabled processing) @@ function
            | true -> "disabled" :: classes
            | false -> classes
        );
      a_onclick (fun _event ->
        Lwt.async (fun () ->
          set_processing true;
          onclick ();%lwt
          set_processing false;
          Lwt.return_unit
        );
        false
      );
    ]
    (
      make_content
        ?label
        ?label_processing
        ?icon
        ?badge
        ~processing
        ()
    )

let make_a
    ?label
    ?label_processing
    ?icon
    ?badge
    ?(disabled = S.const false)
    ?classes
    ~href
    ?(more_a = [])
    ()
  =
  a
    ~a: (
      [R.a_class
        (
          let classes = "btn" :: Option.value ~default: [] classes in
          Fun.flip S.map disabled @@ function
            | true -> "disabled" :: classes
            | false -> classes
        );
      R.a_href href;
      ] @
        more_a
    )
    (
      make_content
        ?label
        ?label_processing
        ?icon
        ?badge
        ()
    )

let make_icon ?(classes = []) icon =
  button
    ~a: [a_button_type `Button; a_class (["btn"; "disabled"] @ classes); a_tabindex (-1)]
    [
      i ~a: [a_class ["bi"; "bi-" ^ icon]] []
    ]

let save ?disabled ~onclick () =
  make
    ~label: "Save"
    ~label_processing: "Saving..."
    ~icon: "save"
    ~classes: ["btn-primary"]
    ?disabled
    ~onclick
    ()

let clear ~onclick () =
  make
    ~label: "Clear"
    ~label_processing: "Clearing..."
    ~icon: "x-lg"
    ~classes: ["btn-warning"]
    ~onclick: (fun () ->
      if Dom_html.window##confirm (Js.string "Clear the editor?") |> Js.to_bool then
        onclick ();
      Lwt.return_unit
    )
    ()

let cancel ~onclick () =
  make
    ~label: "Cancel"
    ~label_processing: "Cancelling..."
    ~icon: "x-lg"
    ~classes: ["btn-secondary"]
    ~onclick
    ()

let cancel' ~return () =
  cancel ~onclick: (fun () -> return None; Lwt.return_unit) ()

let download ~href () =
  make_a
    ~label: "Download"
    ~icon: "download"
    ~classes: ["btn-primary"]
    ~more_a: [a_target "_blank"]
    ~href
    ()
