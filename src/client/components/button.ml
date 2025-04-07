open Js_of_ocaml
open Html

let make
    ~label
    ?label_processing
    ?icon
    ?badge
    ~classes
    ?(disabled = S.const false)
    ~onclick
    ()
  =
  let classes = "btn" :: classes in
  let (processing, set_processing) = React.S.create false in
  button
    (
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
                      | true -> ["spinner-border"; "spinner-border-sm"]
                      | false -> ["d-none"]
                    );
                  a_aria "hidden" ["true"]
                ]
                [];
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
                      | false -> ["bi"; "bi-" ^ icon]
                    )
                ]
                [];
              txt " ";
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
              txt " ";
              span ~a: [a_class ["badge"; "text-bg-secondary"]] [txt badge];
            ]
          )
        ]
    )
    ~a: [
      a_button_type `Button;
      R.a_class
        (
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
