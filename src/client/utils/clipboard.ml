open Js_of_ocaml
open Html

let write_to_clipboard text : unit =
  (* NOTE: Ideally, we would use the Navigator API and the JS function
     `navigator.clipboard.writeText`; but it does not seem present in
     Js_of_ocaml as of 6.2.0 (August 2025). We use the `execCommand("copy")`
     technique instead. *)
  (* Dom_html.window##.navigator##.clipboard##.writeText (Js.string text); *)
  let holder =
    (* We need an element in which we can focus the user and select everything.
       It cannot be `display: none` but we of course do not want to see it. *)
    To_dom.of_textarea @@
      textarea
        ~a: [a_style "position: absolute; width: 0; height: 0;"]
        (txt text)
  in
  Dom.appendChild Dom_html.document##.body holder;
  holder##focus;
  holder##select;
  Dom_html.document##execCommand (Js.string "copy") Js._false Js.null;
  Dom.removeChild Dom_html.document##.body holder
