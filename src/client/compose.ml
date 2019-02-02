open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Composer = struct

  type t = {
    mutable tunes : Dancelor_model.Tune.t option array;
    mutable count : int;
  }

  let create () = 
    {
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
    if i >= 0 && i < t.count then 
      t.tunes.(i) <- None

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

  let save _composer = 
    Html.window##alert (js "Saved (actually it's not)")

  let rec refresh (tunes_area : Html.divElement Js.t) composer = 
    let document = Html.window##.document in
    while Js.to_bool tunes_area##hasChildNodes do
      let child = Js.Opt.get tunes_area##.firstChild (fun () -> assert false) in
      tunes_area##removeChild child
      |> ignore
    done;
    Composer.iter composer (fun index tune ->
      let tune_group = Dancelor_model.Tune.group tune in
      let tune_name = Dancelor_model.TuneGroup.name tune_group in
      let tune_slug = Dancelor_model.Tune.slug tune in
      let html_image = Html.createImg document in
      html_image##.src := js (Printf.sprintf "/tune/%s.png" tune_slug);
      let up_button = Html.createButton ~_type:(js "button") document in
      up_button##.className := js "btn btn-default"; 
      up_button##.textContent := Js.some (js "Monter");
      Lwt.async (fun () ->
        Lwt_js_events.clicks up_button 
          (fun _ev _ -> 
            Composer.move_up composer index; 
            refresh tunes_area composer;
            Lwt.return ()));
      let down_button = Html.createButton ~_type:(js "button") document in
      down_button##.className := js "btn btn-default"; 
      down_button##.textContent := Js.some (js "Descendre");
      Lwt.async (fun () ->
        Lwt_js_events.clicks down_button 
          (fun _ev _ -> 
            Composer.move_down composer index; 
            refresh tunes_area composer;
            Lwt.return ()));
      let del_button = Html.createButton ~_type:(js "button") document in
      del_button##.className := js "btn btn-danger"; 
      del_button##.textContent := Js.some (js "Supprimer");
      Lwt.async (fun () ->
        Lwt_js_events.clicks del_button 
          (fun _ev _ -> 
            Composer.remove composer index; 
            refresh tunes_area composer;
            Lwt.return ()));
      Dom.appendChild tunes_area (document##createTextNode (js tune_name));
      Dom.appendChild tunes_area (Html.createBr document);
      Dom.appendChild tunes_area html_image;
      Dom.appendChild tunes_area (Html.createBr document);
      Dom.appendChild tunes_area up_button;
      Dom.appendChild tunes_area down_button;
      Dom.appendChild tunes_area del_button;
      Dom.appendChild tunes_area (Html.createHr document);
    )

  let make_search_result composer tunes_area tune score =
    let document = Html.window##.document in
    let tr = Html.createTr document in
    let tune_group = Dancelor_model.Tune.group tune in
    let tune_name = Dancelor_model.TuneGroup.name tune_group in
    let tune_bars = Dancelor_model.Tune.bars tune in
    let tune_kind = Dancelor_model.TuneGroup.kind tune_group in
    let tune_structure = Dancelor_model.Tune.structure tune in
    let make_entry s = 
      let td = Html.createTd document in
      td##.textContent := Js.some (js s);
      td
    in
    let score_str = string_of_int (int_of_float score) in
    let bars_kind_str = 
      Printf.sprintf "%i %s" 
        tune_bars (Dancelor_model.Kind.base_to_string tune_kind)
    in
    let add_link = Html.createA document in
    add_link##.href := js "#";
    add_link##.textContent := Js.some (js "Add");
    Lwt.async (fun () ->
      Lwt_js_events.clicks add_link
        (fun _ev _ -> 
          Composer.add composer tune; 
          refresh tunes_area composer;
          Lwt.return ()));
    let link_entry = Html.createTd document in
    Dom.appendChild link_entry add_link;
    Dom.appendChild tr (make_entry score_str);
    Dom.appendChild tr (make_entry tune_name);
    Dom.appendChild tr (make_entry bars_kind_str);
    Dom.appendChild tr (make_entry tune_structure);
    Dom.appendChild tr link_entry;
    tr

  let search_tunes composer tunes_area search_results input = 
    let document = Html.window##.document in
    while Js.to_bool search_results##hasChildNodes do
      let child = Js.Opt.get search_results##.firstChild (fun () -> assert false) in
      search_results##removeChild child
      |> ignore
    done;
    let results = Dancelor_model.Tune.Database.get_all ~name:input () in
    let table = Html.createTable document in
    List.iter (fun (score, tune) ->
      let result_tr = make_search_result composer tunes_area tune score in
      Dom.appendChild table result_tr
    ) results;
    Dom.appendChild search_results table

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
    refresh tunes_area composer;
    Dom.appendChild form tunes_area;
    let save_button = Html.createButton ~_type:(js "button") document in
    save_button##.className := js "btn btn-success"; 
    save_button##.textContent := Js.some (js "Save");
    Lwt.async (fun () ->
      Lwt_js_events.clicks save_button 
        (fun _ev _ -> save composer; Lwt.return ()));
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
    Lwt.async (fun () ->
      Lwt_js_events.keyups search_bar
        (fun _ev _ -> 
          let input = Js.to_string search_bar##.value in
          search_tunes composer tunes_area search_results input;
          Lwt.return ()));

end

let on_load _ev = 
  let composer = Composer.create () in
  Interface.create composer;
  Js._false

let _ = 
  Html.window##.onload := Html.handler on_load
