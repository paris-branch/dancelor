open Js_of_ocaml
open Dancelor_client_html

let group content =
  div ~a:[a_style "display: flex;"; a_class ["justify-content-space-between"]] content

let save ~disabled ~onclick () =
  button
    [
      i ~a:[a_class ["material-symbols-outlined"]] [txt "save"];
      txt " Save";
    ]
    ~a: [
      R.a_class (
        Fun.flip S.map disabled @@ function
        | true -> ["btn-success"; "disabled"]
        | false -> ["btn-success"]);
      a_onclick (fun _event ->
          Lwt.async onclick;
          false
        );
    ]

let clear ~onclick () =
  button
    [
      i ~a:[a_class ["material-symbols-outlined"]] [txt "cancel"];
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
