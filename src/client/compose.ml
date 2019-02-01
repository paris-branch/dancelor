open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let document = Html.window##.document

let () = 
  print_endline "OK"

let content = 
  Js.Opt.get (document##getElementById (js "content"))
    (fun () -> assert false)

let foo = Html.createP document

let () = 
  foo##.textContent := Js.some (Js.string "C'est spartiate parce que c'est en cours de composition ;-)");
  Dom.appendChild content foo

let form = Html.createForm document

let () = 
  Dom.appendChild content form

let set_name = Html.createInput ~_type:(js "text") document

let () = 
  set_name##.className := js "form-control"; 
  set_name##.id := js "name";
  set_name##.placeholder := js "Set Name";
  Dom.appendChild form set_name;
  Dom.appendChild form (Html.createBr document)

let set_kind = Html.createInput ~_type:(js "text") document

let () = 
  set_kind##.className := js "form-control"; 
  set_kind##.id := js "kind";
  set_kind##.placeholder := js "Set Kind (eg. 8x32R)";
  Dom.appendChild form set_kind;
  Dom.appendChild form (Html.createHr document)

let tunes_area = Html.createDiv document

let () = 
  tunes_area##.id := js "tunesArea";
  Dom.appendChild form tunes_area

let save () = 
  Html.window##alert (js "Saved")

let save_button = Html.createButton ~_type:(js "button") document

let () = 
  save_button##.className := js "btn btn-success"; 
  save_button##.textContent := Js.some (js "Save");
  Lwt.async (fun () ->
    Lwt_js_events.clicks save_button (fun _ev _ -> save (); Lwt.return ()));
  Dom.appendChild form save_button;
  Dom.appendChild form (Html.createHr document)

let search_results = Html.createDiv document

let () = 
  search_results##.id := js "searchResults";
  Dom.appendChild form search_results

(*
  <input type="text" class="form-control" id="search" placeholder="Search for a tune (press enter to apply search)" />
*)


