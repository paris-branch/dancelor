open Js_of_ocaml
open Dancelor_client_html

let group content =
  div
    ~a: [
      a_style "display: flex;";
      a_class ["justify-content-space-between"; "form-element"];
    ]
    content

let save ~disabled ~onclick () =
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
              | false -> "save"
            )
        ];
      txt " ";
      R.txt
        (
          Fun.flip S.map processing @@ function
          | true -> "Saving..."
          | false -> "Save"
        );
    ]
    ~a: [
      R.a_class
        (
          Fun.flip S.map (S.l2 (||) disabled processing) @@ function
          | true -> ["btn-success"; "disabled"]
          | false -> ["btn-success"]
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

let clear ~onclick () =
  button
    [
      i ~a: [a_class ["material-symbols-outlined"]] [txt "cancel"];
      txt " Clear";
    ]
    ~a: [
      a_class ["btn-danger"];
      a_onclick (fun _event ->
          if Dom_html.window##confirm (Js.string "Clear the editor?") |> Js.to_bool then
            onclick ();
          false
        );
    ]
