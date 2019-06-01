open Js_of_ocaml
open Js_of_ocaml_lwt

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
    root##.classList##add (js "clickable");
    root##.classList##add (js "no-selection");
    root##.classList##add (js "unchecked");
    Lwt.async (fun () ->
      Lwt_js_events.changes box
        (fun _ev _ ->
          if Js.to_bool box##.checked then begin
            root##.classList##add (js "checked");
            root##.classList##remove (js "unchecked");
            on_change true
          end else begin
            root##.classList##add (js "unchecked");
            root##.classList##remove (js "checked");
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

module Text = struct

  type root = Html.inputElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let set_valid t b =
    if b then t.root##.classList##remove (js "invalid")
    else t.root##.classList##add (js "invalid")

  let create ?default ?on_change ?on_focus page =
    let root = Html.createInput ~_type:(js "text") (Page.document page) in
    NesOption.ifsome (fun t -> root##.placeholder := js t) default;
    NesOption.ifsome (fun cb ->
      Lwt.async (fun () ->
        Lwt_js_events.inputs root
          (fun _ev _ -> cb (Js.to_string root##.value); Lwt.return ())))
      on_change;
    Lwt.async (fun () ->
      Lwt_js_events.inputs root
        (fun _ev _ -> root##.classList##remove (js "invalid"); Lwt.return ()));
    NesOption.ifsome (fun cb ->
      Lwt.async (fun () ->
        Lwt_js_events.focuses root (fun _ev _ -> cb true; Lwt.return ()));
      Lwt.async (fun () ->
        Lwt_js_events.blurs root (fun _ev _ -> cb false; Lwt.return ())))
      on_focus;
    {page; root}

  let on_change t cb =
    Lwt.async (fun () ->
      Lwt_js_events.inputs t.root
        (fun _ev _ -> cb (Js.to_string t.root##.value); Lwt.return ()))

  let set_contents t c =
    t.root##.value := js c

  let erase t =
    set_contents t ""

  let contents t =
    Js.to_string t.root##.value

  let check t checker =
    let v = checker (contents t) in
    set_valid t v;
    v

  let root t =
    t.root

end

module Button = struct

  module Kind = struct

    type t =
      | Danger
      | Success

    let to_class = function
      | Danger -> "btn-danger"
      | Success -> "btn-success"

  end

  type root = Html.buttonElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let create ~on_click ?kind ?text ?icon page =
    let root = Html.createButton ~_type:(js "button") (Page.document page) in
    Lwt.async (fun () ->
      Lwt_js_events.clicks root
        (fun _ev _ -> on_click (); Lwt.return ()));
    NesOption.ifsome (fun k -> root##.classList##add (js (Kind.to_class k))) kind;
    NesOption.ifsome (fun icon -> Dom.appendChild root (Fa.i icon page)) icon;
    NesOption.ifsome (fun text -> Dom.appendChild root ((Page.document page)##createTextNode (js (" " ^ text)))) text;
    root##.classList##add (js "no-selection");
    {page; root}

  let set_enabled t b =
    if b then t.root##.classList##remove (js "disabled")
    else t.root##.classList##add (js "disabled")

  let root t =
    t.root

end
