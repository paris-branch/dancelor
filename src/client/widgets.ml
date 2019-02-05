open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let remove_children elt = 
  while Js.to_bool elt##hasChildNodes do
    let child = Js.Opt.get elt##.firstChild (fun () -> assert false) in
    elt##removeChild child |> ignore
  done

let set_classes elt classes =
  Option.ifsome (List.iter (fun c -> elt##.classList##add (js c))) classes

let form ~parent ~document () =
  let form = Html.createForm document in
  Dom.appendChild parent form;
  form

let hr ~parent ~document () =
  Dom.appendChild parent (Html.createHr document)

let br ~parent ~document () =
  Dom.appendChild parent (Html.createBr document)

let div ?id ~parent ~document () = 
  let div = Html.createDiv document in
  Option.ifsome (fun i -> div##.id := js i) id;
  Dom.appendChild parent div;
  div

let text_input ?placeholder ?classes ?id ~parent ~document () =
  let input = Html.createInput ~_type:(js "text") document in
  Option.ifsome (fun p -> input##.placeholder := js p) placeholder;
  Option.ifsome (fun i -> input##.id := js i) id;
  set_classes input classes;
  Dom.appendChild parent input;
  input

let button ?classes ?text ?callback ~parent ~document () = 
  let button = Html.createButton ~_type:(js "button") document in
  set_classes button classes;
  Option.ifsome (fun t -> button##.textContent := Js.some (js t)) text;
  Option.ifsome (fun callback ->
    Lwt.async (fun () ->
      Lwt_js_events.clicks button
        (fun _ _ -> callback (); Lwt.return ()))) callback;
  Dom.appendChild parent button;
  button

let image ?src ~parent ~document () =
  let html_image = Html.createImg document in
  Option.ifsome (fun src -> html_image##.src := js src) src;
  Dom.appendChild parent html_image;
  html_image

let textnode ~text ~parent ~document () =
  let text_node = document##createTextNode (js text) in
  Dom.appendChild parent text_node
