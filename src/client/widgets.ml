open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Utils = struct

  let remove_children elt = 
    while Js.to_bool elt##hasChildNodes do
      let child = Js.Opt.get elt##.firstChild (fun () -> assert false) in
      elt##removeChild child |> ignore
    done
  
  let add_classes elt classes =
    List.iter (fun c -> elt##.classList##add (js c)) classes
  
  let rem_classes elt classes = 
    List.iter (fun c -> elt##.classList##remove (js c)) classes
  
  let set_parent elt parent = 
    Dom.appendChild parent elt

  let is_focused document elt = 
    document##.activeElement = Js.Opt.return (elt :> Html.element Js.t)

  let destroy (elt : Html.element Js.t) = 
    Js.Opt.case elt##.parentNode 
      (fun () -> ())
      (fun parent -> Dom.removeChild parent elt)

end

module Elements = struct

  let form ?id ?parent ~document () =
    let form = Html.createForm document in
    Option.ifsome (fun i -> form##.id := js i) id;
    Option.ifsome (Utils.set_parent form) parent;
    form
  
  let hr ?parent ~document () =
    let hr = Html.createHr document in
    Option.ifsome (Utils.set_parent hr) parent
  
  let br ?parent ~document () =
    let br = Html.createBr document in
    Option.ifsome (Utils.set_parent br) parent
  
  let div ?id ?classes ?parent ~document () = 
    let div = Html.createDiv document in
    Option.ifsome (fun i -> div##.id := js i) id;
    Option.ifsome (Utils.add_classes div) classes;
    Option.ifsome (Utils.set_parent div) parent;
    div
  
  let table ?id ?classes ?parent ~document () = 
    let table = Html.createTable document in
    Option.ifsome (fun i -> table##.id := js i) id;
    Option.ifsome (Utils.add_classes table) classes;
    Option.ifsome (Utils.set_parent table) parent;
    table

  let tr ?id ?classes ?parent ~document () = 
    let tr = Html.createTr document in
    Option.ifsome (fun i -> tr##.id := js i) id;
    Option.ifsome (Utils.add_classes tr) classes;
    Option.ifsome (Utils.set_parent tr) parent;
    tr

  let td ?id ?classes ?parent ?text ~document () = 
    let td = Html.createTd document in
    Option.ifsome (fun i -> td##.id := js i) id;
    Option.ifsome (Utils.add_classes td) classes;
    Option.ifsome (Utils.set_parent td) parent;
    Option.ifsome (fun s -> td##.textContent := Js.some (js s)) text;
    td

  let text_input ?placeholder ?classes ?id ?parent ~document () =
    let input = Html.createInput ~_type:(js "text") document in
    Option.ifsome (fun p -> input##.placeholder := js p) placeholder;
    Option.ifsome (fun i -> input##.id := js i) id;
    Option.ifsome (Utils.add_classes input) classes;
    Option.ifsome (Utils.set_parent input) parent;
    input
  
  let button ?classes ?text ?callback ?parent ~document () = 
    let button = Html.createButton ~_type:(js "button") document in
    Option.ifsome (Utils.add_classes button) classes;
    Option.ifsome (fun t -> button##.textContent := Js.some (js t)) text;
    Option.ifsome (fun callback ->
      Lwt.async (fun () ->
        Lwt_js_events.clicks button
          (fun _ _ -> callback (); Lwt.return ()))) callback;
    Option.ifsome (Utils.set_parent button) parent;
    button
  
  let image ?classes ?src ?parent ~document () =
    let html_image = Html.createImg document in
    Option.ifsome (fun src -> html_image##.src := js src) src;
    Option.ifsome (Utils.add_classes html_image) classes;
    Option.ifsome (Utils.set_parent html_image) parent;
    html_image
  
  let textnode ~text ?parent ~document () =
    let text_node = document##createTextNode (js text) in
    Option.ifsome (Utils.set_parent text_node) parent

  let li ?text ?parent ~document () =
    let li = Html.createLi document in
    Option.ifsome (fun t -> li##.textContent := Js.some (js t)) text;
    Option.ifsome (Utils.set_parent li) parent;
    li

  let dropdown ?classes ?id ~entries ~parent ~document () =
    let ul = Html.createUl document in
    Option.ifsome (Utils.add_classes ul) classes;
    Option.ifsome (fun i -> ul##.id := js i) id;
    Utils.add_classes ul ["my-dropdown-menu"];
    Utils.set_parent ul parent;
    List.iter (fun (entry, callback) ->
      Utils.add_classes entry ["my-dropdown-entry"];
      Utils.set_parent entry ul;
      Lwt.async (fun () ->
        Lwt_js_events.clicks entry
          (fun _ev _ -> callback (); Lwt.return ())))
      entries;
    ul

end

module SearchBar = struct

  type t = {
    document : Html.document Js.t;
    main_div : Html.divElement Js.t;
    search_bar : Html.inputElement Js.t;
    search_results : Html.tableElement Js.t;
    mutable has_results : bool;
    mutable shown : bool;
    mutable hovered : bool;
  }

  let hide t = 
    if t.shown && (not t.hovered) 
    && not (Utils.is_focused t.document t.search_bar) then begin
      Dom.removeChild t.main_div t.search_results;
      t.shown <- false
    end

  let show t = 
    if (not t.shown) && t.has_results 
    && Utils.is_focused t.document t.search_bar then begin
      Dom.appendChild t.main_div t.search_results;
      t.shown <- true
    end

  let create ?id ?placeholder ?parent ~document () =
    let main_div = 
      Elements.div ~classes:["search-area"] ?id ?parent ~document () 
    in
    let search_bar = 
      Elements.text_input ~classes:["search-bar"] ~parent:main_div ?placeholder 
        ~document () 
    in
    let search_results = 
      Elements.table ~classes:["search-results"] ~document () 
    in
    let t = {
      document;
      main_div; 
      search_bar; 
      search_results; 
      has_results = false;
      shown = false;
      hovered = false} 
    in
    Lwt.async (fun () ->
      Lwt_js_events.focuses t.search_bar
        (fun _ _ -> show t; Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.blurs t.search_bar
        (fun _ _ -> hide t; Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.mouseovers t.main_div
        (fun _ _ -> t.hovered <- true; Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.mouseouts t.main_div
        (fun _ _ -> t.hovered <- false; hide t; Lwt.return ()));
    t

  let main_div t = t.main_div

  let contents t = 
    t.search_bar##.value |> Js.to_string
  
  let set_parent t p = 
    Utils.set_parent t.main_div p

  let add_result t ?callback elt =
    t.has_results <- true;
    show t;
    Utils.set_parent elt t.search_results;
    Utils.add_classes elt ["search-result"];
    match callback with
    | None -> ()
    | Some cb ->
      Lwt.async (fun () ->
        Lwt_js_events.clicks elt
          (fun _ev _ -> cb (); t.hovered <- false; hide t; Lwt.return ()))

  let rem_results t = 
    t.has_results <- false;
    hide t;
    Utils.remove_children t.search_results

  let on_update t callback = 
     Lwt.async (fun () ->
      Lwt_js_events.keyups t.search_bar
        (fun _ev _ -> callback (); Lwt.return ()))

end
