open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Toggle = struct

  type root = Html.labelElement

  type t = {
    page : Page.t;
    root : root Js.t;
    box : Html.inputElement Js.t
  }

  let create ~text ~id ~on_change page =
    let root = Html.createLabel (Page.document page) in
    let box = Html.createInput ~_type:(js "checkbox") (Page.document page) in
    box##.id := js id;
    root##.textContent := Js.some (js text);
    Dom.appendChild root box;
    root##.htmlFor := js id;
    root##.classList##add (js "toggle-button");
    root##.classList##add (js "no-selection");
    Lwt.async (fun () ->
      Lwt_js_events.changes box
        (fun _ev _ -> 
          if Js.to_bool box##.checked then begin
            Style.set ~color:"#ced4da" root;
            on_change true
          end else begin
            Style.set ~color:"#eeeeee" root;
            on_change false
          end;
          Lwt.return ()));
    {page; root; box}

  let disable t = 
    t.box##.disabled := Js._true

  let enable t = 
    t.box##.disabled := Js._false

  let checked t = 
    t.box##.checked

  let root t = 
    t.root


end
