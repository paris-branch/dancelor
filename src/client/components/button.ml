open Nes
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
                    flip S.map processing @@ function
                      | true -> ["spinner-border"; "spinner-border-sm"; (if label <> "" then "me-2" else "me-0")]
                      | false -> ["d-none"]
                  );
                a_aria "hidden" ["true"]
              ]
              [];
          ];
        (
          flip Option.map icon @@ fun icon ->
          [
            i
              ~a: [
                R.a_class
                  (
                    flip S.map processing @@ function
                      | true -> ["d-none"]
                      | false -> ["bi"; "bi-" ^ icon; (if label <> "" then "me-2" else "me-0")]
                  )
              ]
              [];
          ]
        );
        Some
          [
            R.txt
              (
                flip S.map processing @@ function
                  | true -> Option.value ~default: label label_processing
                  | false -> label
              );
          ];
        (
          flip Option.map badge @@ fun badge ->
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
    ?onclick
    ?(more_a = [])
    ()
  =
  let (processing, set_processing) = React.S.create false in
  button
    ~a: (
      List.flatten
        [
          [a_button_type `Button];
          [R.a_class @@
            let classes = "btn" :: Option.value ~default: [] classes in
            flip S.map (S.l2 (||) disabled processing) @@ function
              | true -> "disabled" :: classes
              | false -> classes];
          (
            match onclick with
            | None -> []
            | Some onclick ->
              [
                a_onclick @@ fun _event ->
                Lwt.async (fun () ->
                  set_processing true;
                  onclick ();%lwt
                  set_processing false;
                  lwt_unit
                );
                false
              ]
          );
          more_a;
        ]
    )
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
          flip S.map disabled @@ function
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
    ~icon: "eraser"
    ~classes: ["btn-warning"]
    ~onclick: (fun () ->
      if Dom_html.window##confirm (Js.string "Clear the editor?") |> Js.to_bool then
        onclick ();
      lwt_unit
    )
    ()

let cancel ?onclick ?more_a () =
  make
    ~label: "Cancel"
    ~label_processing: "Cancelling..."
    ~icon: "x-lg"
    ~classes: ["btn-secondary"]
    ?onclick
    ?more_a
    ()

let cancel' ~return ?more_a () =
  cancel
    ~onclick: (fun () -> return None; lwt_unit)
    ?more_a
    ()

let close ?onclick ?more_a () =
  make
    ~label: "Close"
    ~label_processing: "Closing..."
    ~icon: "x-lg"
    ~classes: ["btn-secondary"]
    ?onclick
    ?more_a
    ()

let close' ~return ?more_a () =
  close
    ~onclick: (fun () -> return None; lwt_unit)
    ?more_a
    ()

let ok ?onclick ?more_a () =
  make
    ~label: "OK"
    ~label_processing: "Closing..."
    ~classes: ["btn-primary"]
    ?onclick
    ?more_a
    ()

let ok' ~return ?more_a () =
  ok
    ~onclick: (fun () -> return (Some ()); lwt_unit)
    ?more_a
    ()

let download ~href () =
  make_a
    ~label: "Download"
    ~icon: "download"
    ~classes: ["btn-primary"]
    ~more_a: [a_target "_blank"]
    ~href
    ()
