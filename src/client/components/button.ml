open Js_of_ocaml
open Dancelor_client_html

let group content =
  div
    ~a: [
      a_style "display: flex;";
      a_class ["justify-content-space-between"; "form-element"];
    ]
    content

let make
    ~label
    ~label_processing
    ~icon
    ~classes
    ?(disabled = S.const false)
    ~onclick
    ()
  =
  let (processing, set_processing) = React.S.create false in
  button
    [
      i
        ~a: [a_class ["material-symbols-outlined"]]
        [
          R.txt
            (
              Fun.flip S.map processing @@ function
                | true -> "pending"
                | false -> icon
            )
        ];
      txt " ";
      R.txt
        (
          Fun.flip S.map processing @@ function
            | true -> label_processing
            | false -> label
        );
    ]
    ~a: [
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
    ~classes: ["btn-success"]
    ?disabled
    ~onclick
    ()

let clear ~onclick () =
  make
    ~label: "Clear"
    ~label_processing: "Clearing..."
    ~icon: "cancel"
    ~classes: ["btn-danger"]
    ~onclick: (fun () ->
      if Dom_html.window##confirm (Js.string "Clear the editor?") |> Js.to_bool then
        onclick ();
      Lwt.return_unit
    )
    ()

let cancel ~return () =
  make
    ~label: "Cancel"
    ~label_processing: "Cancelling..."
    ~icon: "cancel"
    ~classes: ["btn-danger"]
    ~onclick: (fun () ->
      return (Error Dialog.Closed);
      Lwt.return_unit
    )
    ()
