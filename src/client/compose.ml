open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Composer = struct

  type t = {
    mutable name : string;
    mutable kind : string;
    mutable tunes : Dancelor_model.Tune.t option array;
    mutable count : int;
  }

  let create () =
    {
      name = "";
      kind = "";
      tunes = Array.make 2 None;
      count = 0;
    }

  let name t = 
    t.name

  let set_name t name = 
    t.name <- name

  let kind t = 
    t.kind

  let set_kind t kind = 
    t.kind <- kind

  let insert t tune i = 
    if Array.length t.tunes = t.count then begin
      let new_tunes = Array.make (t.count * 2) None in
      Array.blit t.tunes 0 new_tunes 0 t.count;
      t.tunes <- new_tunes;
    end;
    for idx = t.count-1 downto i do
      t.tunes.(idx+1) <- t.tunes.(idx)
    done;
    t.tunes.(i) <- Some tune;
    t.count <- t.count + 1

  let add t tune =
    insert t tune t.count

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
      f i (Option.unwrap t.tunes.(i))
    done

  let fold t f acc = 
    let acc = ref acc in
    for i = t.count - 1 downto 0 do
      acc := f i (Option.unwrap t.tunes.(i)) !acc
    done;
    !acc

  let list_tunes t = 
    fold t (fun _ tune acc -> tune::acc) []

  let save t = 
    Js.Optdef.case Dom_html.window##.localStorage 
      (fun () -> ()) 
      (fun local_storage ->
        let tunes = 
          list_tunes t
          |> List.map Dancelor_model.Tune.slug
          |> String.concat ";"
        in
        local_storage##setItem (js "composer.name") (js t.name);
        local_storage##setItem (js "composer.kind") (js t.kind);
        local_storage##setItem (js "composer.tunes") (js tunes))

  let request_tune_slug slug callback = 
    let path = Dancelor_router.(path_of_controller (TuneSlug slug)) |> snd in
    Helpers.send_request ~path 
      ~callback:(fun str ->
        Dancelor_common.Json.from_string str
        |> Dancelor_common.Json.find ["tune"]
        |> Dancelor_common.Json.of_value
        |> Dancelor_model.Tune.of_json
        |> callback) ()

  let load t cb = 
    Js.Optdef.case Dom_html.window##.localStorage 
      (fun () -> ()) 
      (fun local_storage ->
        let name, kind, tunes = 
          local_storage##getItem (js "composer.name"),
          local_storage##getItem (js "composer.kind"),
          local_storage##getItem (js "composer.tunes")
        in
        Js.Opt.case name (fun () -> ())
          (fun name -> t.name <- Js.to_string name);
        Js.Opt.case kind (fun () -> ())
          (fun kind -> t.kind <- Js.to_string kind);
        Js.Opt.case tunes (fun () -> ())
          (fun tunes -> 
            String.split_on_char ';' (Js.to_string tunes)
            |> List.filter (fun s -> s <> " " && s <> "")
            |> List.iteri (fun idx slug ->
              request_tune_slug slug (fun tune ->
                insert t tune idx; cb ()))))

  let erase_storage _ = 
    Js.Optdef.case Dom_html.window##.localStorage 
      (fun () -> ()) 
      (fun local_storage ->
        local_storage##removeItem (js "composer.name");
        local_storage##removeItem (js "composer.kind");
        local_storage##removeItem (js "composer.tunes"))

  let submit t callback = 
    let save_path = Dancelor_router.(path_of_controller SetSave) |> snd in
    let tunes = 
      fold t (fun _ tune acc ->
        ("tunes", Dancelor_model.Tune.slug tune) :: acc) []
    in
    Helpers.send_request ~path:save_path 
      ~args:(("name", t.name) :: ("kind", t.kind) :: tunes)
      ~callback:(fun str ->
        Dancelor_common.Json.from_string str
        |> Dancelor_common.Json.find ["set"]
        |> Dancelor_common.Json.of_value
        |> Dancelor_model.Set.of_json
        |> callback;
        erase_storage t) ()

end

module Interface = struct

  type t = {
    composer : Composer.t;
    document : Html.document Js.t;
    form : Html.formElement Js.t;
    set_name : Html.inputElement Js.t;
    set_kind : Html.inputElement Js.t;
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
    let form = Widgets.form ~document ~parent:content () in
    let set_name = 
      Widgets.text_input ~classes:["form-control"] ~id:"name" 
        ~placeholder:"Set Name" ~document ~parent:form () 
    in
    Widgets.br ~document ~parent:form ();
    let set_kind = 
      Widgets.text_input ~classes:["form-control"] ~id:"kind" 
        ~placeholder:"Set Kind (eg. 8x32R)" ~document ~parent:form () 
    in
    Widgets.hr ~document ~parent:form ();
    let tunes_area = Widgets.div ~id:"tunesArea" ~document ~parent:form () in
    let save_button = 
      Widgets.button ~classes:["btn"; "btn-success"] ~text:"Save" 
        ~document ~parent:form () 
    in
    Widgets.hr ~document ~parent:form ();
    let search_bar = 
      Widgets.text_input ~classes:["form-control"] ~id:"search"
        ~placeholder:"Search for a tune" ~document ~parent:form ()
    in
    let search_results = 
      Widgets.div ~id:"searchResults" ~document ~parent:form () 
    in
    {
      composer;
      document;
      form;
      set_name;
      set_kind;
      tunes_area;
      search_bar;
      search_results;
      save_button
    }

  let save interface = 
    Composer.submit interface.composer
      (fun set -> 
        let path_to_pdf = 
          Dancelor_router.SetPdf set
          |> Dancelor_router.path_of_controller
          |> snd
        in
        Html.window##.location##.href := js path_to_pdf)

  let rec refresh interface =
    Widgets.remove_children interface.tunes_area;
    Composer.iter interface.composer (fun index tune ->
      let tune_group = Dancelor_model.Tune.group tune in
      let tune_name = Dancelor_model.TuneGroup.name tune_group in
      Widgets.textnode ~text:tune_name ~document:interface.document 
        ~parent:interface.tunes_area ();
      Widgets.br ~document:interface.document ~parent:interface.tunes_area ();
      let src = Dancelor_router.(path_of_controller (TunePng tune)) |> snd in
      Widgets.image ~document:interface.document ~parent:interface.tunes_area
        ~src () |> ignore;
      Widgets.br ~document:interface.document ~parent:interface.tunes_area ();
      Widgets.button ~classes:["btn"; "btn-default"] ~text:"Monter"
        ~document:interface.document ~parent:interface.tunes_area
        ~callback:(fun () ->
          Composer.move_up interface.composer index;
          Composer.save interface.composer;
          refresh interface) () |> ignore;
      Widgets.button ~classes:["btn"; "btn-default"] ~text:"Descendre"
        ~document:interface.document ~parent:interface.tunes_area
        ~callback:(fun () ->
          Composer.move_down interface.composer index;
          Composer.save interface.composer;
          refresh interface) () |> ignore;
      Widgets.button ~classes:["btn"; "btn-danger"] ~text:"Supprimer"
        ~document:interface.document ~parent:interface.tunes_area
        ~callback:(fun () ->
          Composer.remove interface.composer index;
          Composer.save interface.composer;
          refresh interface) () |> ignore;
      Widgets.hr ~document:interface.document ~parent:interface.tunes_area ();
    );
    interface.set_name##.value := js (Composer.name interface.composer);
    interface.set_kind##.value := js (Composer.kind interface.composer)

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
    let score_str = string_of_int score in
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
          Composer.save interface.composer;
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
    let _, tune_path = Dancelor_router.path_of_controller TuneAll in
    Helpers.send_request ~path:tune_path
      ~args:["name", input; "hard-limit", "10"; "threshold", "50"]
      ~callback:(fun str ->
        let open Dancelor_common in
        Widgets.remove_children interface.search_results;
        let table = Html.createTable interface.document in
        Json.from_string str
        |> Json.find ["tunes"]
        |> Json.list (Option.wrap_fun Json.of_value)
        |> Option.unwrap
        |> List.sub 10
        |> List.map (fun json ->
             (Json.find ["score"] json |> Json.int |> Option.unwrap,
              Dancelor_model.Tune.of_json json))
        |> List.iter (fun (score, tune) ->
             let result_tr = make_search_result interface tune score in
             Dom.appendChild table result_tr);
        Dom.appendChild interface.search_results table) ()

  let connect interface =
    Lwt.async (fun () ->
      Lwt_js_events.clicks interface.save_button
        (fun _ev _ -> save interface; Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.keyups interface.search_bar
        (fun _ev _ ->
          let input = Js.to_string interface.search_bar##.value in
          search_tunes interface input;
          Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.keyups interface.set_name
        (fun _ev _ ->
          let input = Js.to_string interface.set_name##.value in
          Composer.set_name interface.composer input;
          Composer.save interface.composer;
          Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.keyups interface.set_kind
        (fun _ev _ ->
          let input = Js.to_string interface.set_kind##.value in
          Composer.set_kind interface.composer input;
          Composer.save interface.composer;
          Lwt.return ()))

end

let on_load _ev =
  let composer = Composer.create () in
  let interface = Interface.create composer in
  Composer.load composer (fun () -> Interface.refresh interface);
  Interface.refresh interface;
  Interface.connect interface;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
