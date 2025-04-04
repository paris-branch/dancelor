open Js_of_ocaml
open Html

let make
    ~label
    ~label_processing
    ~icon
    ~classes
    ?(disabled = S.const false)
    ~onclick
    ()
  =
  let classes = "btn" :: classes in
  let (processing, set_processing) = React.S.create false in
  button
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
      R.txt
        (
          Fun.flip S.map processing @@ function
          | true -> label_processing
          | false -> label
        );
    ]
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

let cancel ?return ?onclick () =
  let onclick =
    match (return, onclick) with
    | Some return, None -> (fun () -> return None; Lwt.return_unit)
    | None, Some onclick -> onclick
    | _ -> invalid_arg "Button.cancel: cannot have both ~return and ~onclick"
  in
  make
    ~label: "Cancel"
    ~label_processing: "Cancelling..."
    ~icon: "x-lg"
    ~classes: ["btn-secondary"]
    ~onclick
    ()
