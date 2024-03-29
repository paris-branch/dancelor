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

module Switch = struct
  type root = Html.labelElement

  type t = {
    page : Page.t;
    root : root Js.t;
    input : Html.inputElement Js.t
  }

  let create ?(text_before="") ?(text_after="") ~id ~on_change page =
    let root = Html.createLabel (Page.document page) in
    root##.classList##add (js "switch");
    root##.htmlFor := js id;
    let text_before = (Page.document page)##createTextNode (Js.string text_before) in
    let text_after = (Page.document page)##createTextNode (Js.string text_after) in
    let box = Html.createSpan (Page.document page) in
    box##.classList##add (js "box");
    Dom.appendChild root text_before;
    Dom.appendChild root box;
    Dom.appendChild root text_after;
    let input = Html.createInput ~_type:(js "checkbox") (Page.document page) in
    input##.id := js id;
    let slider = Html.createSpan (Page.document page) in
    slider##.classList##add (js "slider");
    Dom.appendChild box input;
    Dom.appendChild box slider;
    Lwt.async (fun () ->
        Lwt_js_events.changes box @@ fun _ev _ ->
        on_change (Js.to_bool input##.checked);
        Lwt.return_unit);
    {page; root; input}

  let disable t = t.input##.checked := Js._false
  let enable t = t.input##.checked := Js._true
  let checked t = t.input##.checked
  let root t = t.root
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

  let create ?placeholder ?default ?on_change ?on_focus page =
    let root = Html.createInput ~_type:(js "text") (Page.document page) in
    Option.iter (fun t -> root##.placeholder := js t) placeholder;
    Option.iter (fun t -> root##.value := js t) default;
    Option.iter (fun cb ->
        Lwt.async (fun () ->
            Lwt_js_events.inputs root
              (fun _ev _ -> cb (Js.to_string root##.value); Lwt.return ())))
      on_change;
    Lwt.async (fun () ->
        Lwt_js_events.inputs root
          (fun _ev _ -> root##.classList##remove (js "invalid"); Lwt.return ()));
    Option.iter (fun cb ->
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

  let on_enter t cb =
    Lwt.async (fun () ->
        Lwt_js_events.keydowns t.root
          (fun ev _ ->
             if ev##.keyCode = 13
             then cb (Js.to_string t.root##.value) else Lwt.return_unit))

  let on_focus t cb =
    Lwt.async (fun () ->
        Lwt_js_events.focuses t.root (fun _ev _ -> cb true; Lwt.return ()));
    Lwt.async (fun () ->
        Lwt_js_events.blurs t.root (fun _ev _ -> cb false; Lwt.return ()))

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

module Textarea = struct

  type root = Html.textAreaElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let set_valid t b =
    if b then t.root##.classList##remove (js "invalid")
    else t.root##.classList##add (js "invalid")

  let create ?placeholder ?default ?on_change ?on_focus page =
    let root = Html.createTextarea (Page.document page) in
    Option.iter (fun t -> root##.placeholder := js t) placeholder;
    Option.iter (fun t -> root##.value := js t) default;
    Option.iter (fun cb ->
        Lwt.async (fun () ->
            Lwt_js_events.inputs root
              (fun _ev _ -> cb (Js.to_string root##.value); Lwt.return ())))
      on_change;
    Lwt.async (fun () ->
        Lwt_js_events.inputs root
          (fun _ev _ -> root##.classList##remove (js "invalid"); Lwt.return ()));
    Option.iter (fun cb ->
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

  let on_enter t cb =
    Lwt.async (fun () ->
        Lwt_js_events.keydowns t.root
          (fun ev _ ->
             if ev##.keyCode = 13
             then cb (Js.to_string t.root##.value) else Lwt.return_unit))

  let on_focus t cb =
    Lwt.async (fun () ->
        Lwt_js_events.focuses t.root (fun _ev _ -> cb true; Lwt.return ()));
    Lwt.async (fun () ->
        Lwt_js_events.blurs t.root (fun _ev _ -> cb false; Lwt.return ()))

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

  let create ?(on_click=fun () -> ()) ?href ?kind ?text ?icon page =
    let root = Html.createButton ~_type:(js "button") (Page.document page) in
    Lwt.async (fun () ->
        Lwt_js_events.clicks root
          (fun _ev _ ->
             on_click ();
             match href with
             | None -> Lwt.return ()
             | Some href -> Lwt.map (fun href -> Html.window##.location##.href := js href) href));
    Option.iter (fun k -> root##.classList##add (js (Kind.to_class k))) kind;
    Option.iter (fun icon -> Dom.appendChild root (Fa.i icon page)) icon;
    Option.iter (fun text -> Dom.appendChild root ((Page.document page)##createTextNode (js (" " ^ text)))) text;
    root##.classList##add (js "no-selection");
    root##.classList##add (js "clickable");
    {page; root}

  let set_enabled t b =
    if b then begin
      t.root##.classList##remove (js "disabled");
      t.root##.classList##add (js "clickable")
    end else begin
      t.root##.classList##add (js "disabled");
      t.root##.classList##remove (js "clickable")
    end

  let root t =
    t.root

end
