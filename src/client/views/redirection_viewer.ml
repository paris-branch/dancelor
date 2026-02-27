open Nes
open Js_of_ocaml
open Html

let create target =
  Dom_html.window##.location##.href := Js.string (Uri.to_string target);
  Page.make'
    ~title: (lwt "Redirection")
    [
      div [txt "You are being redirected to:"];
      div [a ~a: [a_href target] [txt @@ Uri.to_string target]];
      div [txt "If you are not redirected automatically, please click the link above."];
    ]
