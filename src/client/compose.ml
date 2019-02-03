open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

(* Unsafe module for JSON manipulation *)
module JSON = struct

  type t

  let json = (Js.Unsafe.variable "JSON")

  let parse j : t =
    let jss = Js.string j in
    Js.Unsafe.meth_call json "parse" [| Js.Unsafe.inject jss |]

  let stringify (obj : t) =
    Js.to_string @@ Js.Unsafe.meth_call json "stringify" [| Js.Unsafe.inject obj |]

  let assoc (obj : t) (field : string) : t =
    Js.Unsafe.get obj (js field)

  let index (obj : t) (field : int) : t =
    Js.Unsafe.get obj (js (string_of_int field))

  let to_array (obj : t) : t Js.js_array Js.t =
    Js.Unsafe.coerce (Js.Unsafe.inject obj)

end

module Composer = struct

  type t = {
    mutable tunes : Dancelor_model.Tune.t option array;
    mutable count : int;
    database : Dancelor_model.Tune.Database.db;
  }

  let create () =
    let _, tune_path = Dancelor_router.path_of_controller TuneAll in
    let database = Dancelor_model.Tune.Database.create () in
    let api = "/" ^ Config.api_prefix ^ tune_path in
    let request = XmlHttpRequest.create () in
    request##.onreadystatechange := Js.wrap_callback (fun () ->
      if request##.readyState = XmlHttpRequest.DONE
      && request##.status = 200 then begin
        let open Dancelor_common in
        let str = request##.responseText |> Js.to_string in
        let json = JSON.parse str in
        let entries = JSON.assoc json "tunes" |> JSON.to_array in
        let db_entries = ref [] in
        entries##forEach (Js.wrap_callback (fun elt _ _ ->
          let json = JSON.stringify elt in
          db_entries := Json.from_string json :: !db_entries));
        Dancelor_model.Tune.Database.fill database !db_entries;
      end);
    request##_open (js "GET") (js api) (Js._true);
    request##send Js.null;
    {
      database;
      tunes = Array.make 2 None;
      count = 0;
    }

  let add t tune =
    if Array.length t.tunes = t.count then begin
      let new_tunes = Array.make (t.count * 2) None in
      Array.blit t.tunes 0 new_tunes 0 t.count;
      t.tunes <- new_tunes;
    end;
    t.tunes.(t.count) <- Some tune;
    t.count <- t.count + 1

  let get t i =
    if i < 0 || i >= t.count then
      None
    else
      t.tunes.(i)

  let remove t i =
    if i >= 0 && i < t.count then begin
      t.tunes.(i) <- None;
      for j = i + 1 to t.count - 1 do
        t.tunes.(j-1) <- t.tunes.(j);
        t.tunes.(j) <- None;
      done;
      t.count <- t.count - 1
    end

  let move_up t i =
    if i > 0 && i < t.count then begin
      let tmp = t.tunes.(i-1) in
      t.tunes.(i-1) <- t.tunes.(i);
      t.tunes.(i) <- tmp
    end

  let move_down t i =
    move_up t (i+1)

  let iter t f =
    for i = 0 to t.count - 1 do
      match t.tunes.(i) with
      | None -> assert false
      | Some tune -> f i tune
    done

end

module Interface = struct

  type t = {
    composer : Composer.t;
    document : Html.document Js.t;
    form : Html.formElement Js.t;
    tunes_area : Html.divElement Js.t;
    search_results : Html.divElement Js.t;
    search_bar : Html.inputElement Js.t;
    save_button : Html.buttonElement Js.t;
  }

  let create composer =
    let document = Html.window##.document in
    let content =
      Js.Opt.get (document##getElementById (js "content"))
        (fun () -> assert false)
    in
    let form = Html.createForm document in
    Dom.appendChild content form;
    let set_name = Html.createInput ~_type:(js "text") document in
    set_name##.className := js "form-control";
		set_name##.id := js "name";
    set_name##.placeholder := js "Set Name";
    Dom.appendChild form set_name;
    Dom.appendChild form (Html.createBr document);
    let set_kind = Html.createInput ~_type:(js "text") document in
    set_kind##.className := js "form-control";
    set_kind##.id := js "kind";
    set_kind##.placeholder := js "Set Kind (eg. 8x32R)";
    Dom.appendChild form set_kind;
    Dom.appendChild form (Html.createHr document);
    let tunes_area = Html.createDiv document in
    tunes_area##.id := js "tunesArea";
    Dom.appendChild form tunes_area;
    let save_button = Html.createButton ~_type:(js "button") document in
    save_button##.className := js "btn btn-success";
    save_button##.textContent := Js.some (js "Save");
    Dom.appendChild form save_button;
    Dom.appendChild form (Html.createHr document);
    let search_bar = Html.createInput ~_type:(js "text") document in
    search_bar##.className := js "form-control";
    search_bar##.id := js "search";
    search_bar##.placeholder := js "Search for a tune";
    Dom.appendChild form search_bar;
		let search_results = Html.createDiv document in
    search_results##.id := js "searchResults";
    Dom.appendChild form search_results;
    {
      composer;
      document;
      form;
      tunes_area;
      search_bar;
      search_results;
      save_button
    }

  let save _interface =
    Html.window##alert (js "Saved (actually it's not)")

  let rec refresh interface =
    while Js.to_bool interface.tunes_area##hasChildNodes do
      let child =
        Js.Opt.get interface.tunes_area##.firstChild (fun () -> assert false)
      in
      interface.tunes_area##removeChild child
      |> ignore
    done;
    Composer.iter interface.composer (fun index tune ->
      let tune_group = Dancelor_model.Tune.group tune in
      let tune_name = Dancelor_model.TuneGroup.name tune_group in
      let tune_slug = Dancelor_model.Tune.slug tune in
      let html_image = Html.createImg interface.document in
      html_image##.src := js (Printf.sprintf "/tune/%s.png" tune_slug);
      let up_button =
        Html.createButton ~_type:(js "button") interface.document
      in
      up_button##.className := js "btn btn-default";
      up_button##.textContent := Js.some (js "Monter");
      Lwt.async (fun () ->
        Lwt_js_events.clicks up_button
          (fun _ev _ ->
            Composer.move_up interface.composer index;
            refresh interface;
            Lwt.return ()));
      let down_button =
        Html.createButton ~_type:(js "button") interface.document
      in
      down_button##.className := js "btn btn-default";
      down_button##.textContent := Js.some (js "Descendre");
      Lwt.async (fun () ->
        Lwt_js_events.clicks down_button
          (fun _ev _ ->
            Composer.move_down interface.composer index;
            refresh interface;
            Lwt.return ()));
      let del_button =
        Html.createButton ~_type:(js "button") interface.document
      in
      del_button##.className := js "btn btn-danger";
      del_button##.textContent := Js.some (js "Supprimer");
      Lwt.async (fun () ->
        Lwt_js_events.clicks del_button
          (fun _ev _ ->
            Composer.remove interface.composer index;
            refresh interface;
            Lwt.return ()));
      let text_node = interface.document##createTextNode (js tune_name) in
      Dom.appendChild interface.tunes_area text_node;
      Dom.appendChild interface.tunes_area (Html.createBr interface.document);
      Dom.appendChild interface.tunes_area html_image;
      Dom.appendChild interface.tunes_area (Html.createBr interface.document);
      Dom.appendChild interface.tunes_area up_button;
      Dom.appendChild interface.tunes_area down_button;
      Dom.appendChild interface.tunes_area del_button;
      Dom.appendChild interface.tunes_area (Html.createHr interface.document);
    )

  let make_search_result interface tune score =
    let tr = Html.createTr interface.document in
    let tune_group = Dancelor_model.Tune.group tune in
    let tune_name = Dancelor_model.TuneGroup.name tune_group in
    let tune_bars = Dancelor_model.Tune.bars tune in
    let tune_kind = Dancelor_model.TuneGroup.kind tune_group in
    let tune_structure = Dancelor_model.Tune.structure tune in
    let make_entry s =
      let td = Html.createTd interface.document in
      td##.textContent := Js.some (js s);
      td
    in
    let score_str = string_of_int (int_of_float (score *. 100.)) in
    let bars_kind_str =
      Printf.sprintf "%i %s"
        tune_bars (Dancelor_model.Kind.base_to_string tune_kind)
    in
    let add_link = Html.createA interface.document in
    add_link##.href := js "#";
    add_link##.textContent := Js.some (js "Add");
    Lwt.async (fun () ->
      Lwt_js_events.clicks add_link
        (fun _ev _ ->
          Composer.add interface.composer tune;
          refresh interface;
          Lwt.return ()));
    let link_entry = Html.createTd interface.document in
    Dom.appendChild link_entry add_link;
    Dom.appendChild tr (make_entry score_str);
    Dom.appendChild tr (make_entry tune_name);
    Dom.appendChild tr (make_entry bars_kind_str);
    Dom.appendChild tr (make_entry tune_structure);
    Dom.appendChild tr link_entry;
    tr

  let search_tunes interface input =
    while Js.to_bool interface.search_results##hasChildNodes do
      let child =
        Js.Opt.get interface.search_results##.firstChild
          (fun () -> assert false)
      in
      interface.search_results##removeChild child
      |> ignore
    done;
    let rec list_sub n l =
      if n <= 0 then []
      else begin
        match l with
        | [] -> []
        | h::t -> h::(list_sub (n-1) t)
      end
    in
    let results =
      Dancelor_model.Tune.Database.get_all
        ~db:interface.composer.Composer.database
        ~name:input ()
      |> list_sub 10
    in
    let table = Html.createTable interface.document in
    List.iter (fun (score, tune) ->
      let result_tr = make_search_result interface tune score in
      Dom.appendChild table result_tr
    ) results;
    Dom.appendChild interface.search_results table

  let connect interface =
    Lwt.async (fun () ->
      Lwt_js_events.clicks interface.save_button
        (fun _ev _ -> save interface; Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.keyups interface.search_bar
        (fun _ev _ ->
          let input = Js.to_string interface.search_bar##.value in
          print_endline "Searching";
          search_tunes interface input;
          Lwt.return ()))

end

let on_load _ev =
  let composer = Composer.create () in
  let interface = Interface.create composer in
  Interface.connect interface;
  Interface.refresh interface;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
