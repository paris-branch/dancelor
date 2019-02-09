open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Composer = struct

  type t = {
    state : [`Edit of Slug.t | `Create];
    mutable name : string;
    mutable kind : string;
    mutable tunes : Dancelor_model.Tune.t option array;
    mutable count : int;
  }

  let create () =
    {
      state = `Create;
      name = "";
      kind = "";
      tunes = Array.make 2 None;
      count = 0;
    }

  let edit set = 
    let tunes_list = Dancelor_model.Set.tunes set in
    let count = List.length tunes_list in
    let tunes = Array.make (max 2 count) None in
    List.iteri (fun i t -> tunes.(i) <- Some t) tunes_list;
    {
      state = `Edit (Dancelor_model.Set.slug set);
      name = Dancelor_model.Set.name set;
      kind = Dancelor_model.Set.kind set |> Dancelor_model.Kind.dance_to_string;
      tunes;
      count
    }

  let name t = 
    t.name

  let set_name t name = 
    t.name <- name

  let kind t = 
    t.kind

  let set_kind t kind = 
    t.kind <- kind

  let count t = 
    t.count

  let insert t tune i = 
    if Array.length t.tunes = t.count then begin
      let new_tunes = Array.make (t.count * 2) None in
      Array.blit t.tunes 0 new_tunes 0 t.count;
      t.tunes <- new_tunes;
    end;
    for idx = t.count-1 downto i do
      t.tunes.(idx+1) <- t.tunes.(idx)
    done;
    t.tunes.(min t.count i) <- Some tune;
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
      match t.tunes.(i) with
      | None -> ()
      | Some tune -> f i tune
    done

  let fold t f acc = 
    let acc = ref acc in
    for i = t.count - 1 downto 0 do
      match t.tunes.(i) with
      | None -> ()
      | Some tune -> acc := f i tune !acc
    done;
    !acc

  let list_tunes t = 
    fold t (fun _ tune acc -> tune::acc) []

  let clear t = 
    t.name <- "";
    t.kind <- "";
    t.count <- 0

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

  module Controls = struct

    type t = {
      main : Html.divElement Js.t;
      save : Html.buttonElement Js.t;
      clear : Html.buttonElement Js.t;
      mutable shown : bool;
    }

    let create ~document ~parent =
      let main = 
        Widgets.Elements.div ~classes:["controls"] ~document ~parent () 
      in
      let save = 
        Widgets.Elements.button ~classes:["btn"; "btn-success"] ~text:"Save" 
          ~document ()
      in
      let clear = 
        Widgets.Elements.button ~classes:["btn"; "btn-danger"] ~text:"Clear" 
          ~document ()
      in
      {main; save; clear; shown = false}

    let main t = t.main

    let bind_save t cb = 
      Lwt.async (fun () ->
        Lwt_js_events.clicks t.save
          (fun _ev _ -> cb (); Lwt.return ()))

    let bind_clear t cb = 
      Lwt.async (fun () ->
        Lwt_js_events.clicks t.clear
          (fun _ev _ -> cb (); Lwt.return ()))

    let show t = 
      if not t.shown then begin
        Widgets.Utils.set_parent t.save t.main;
        Widgets.Utils.set_parent t.clear t.main;
        t.shown <- true
      end

    let hide t = 
      if t.shown then begin
        Widgets.Utils.remove_children t.main;
        t.shown <- false
      end
    
  end

  type t = {
    composer : Composer.t;
    document : Html.document Js.t;
    form : Html.formElement Js.t;
    set_name : Html.inputElement Js.t;
    set_kind : Html.inputElement Js.t;
    tunes_area : Html.divElement Js.t;
    search_bar : Widgets.SearchBar.t;
    controls : Controls.t;
  }

  let create composer parent =
    let document = Html.window##.document in
    let form = Widgets.Elements.form ~document ~parent ~id:"composer" () in
    let set_name = 
      Widgets.Elements.text_input ~classes:["form-control"] ~id:"name" 
        ~placeholder:"Set Name" ~document ~parent:form () 
    in
    Widgets.Elements.br ~document ~parent:form ();
    let set_kind = 
      Widgets.Elements.text_input ~classes:["form-control"] ~id:"kind" 
        ~placeholder:"Set Kind (eg. 8x32R)" ~document ~parent:form () 
    in
    Widgets.Elements.br ~document ~parent:form ();
    let tunes_area = Widgets.Elements.div ~id:"tunesArea" ~document ~parent:form () in
    let search_bar = 
      Widgets.SearchBar.create ~placeholder:"Search for a tune"
        ~document ~parent:form ()
    in
    Widgets.Elements.br ~document ~parent:form ();
    let controls = Controls.create ~document ~parent:form in
    {
      composer;
      document;
      form;
      set_name;
      set_kind;
      tunes_area;
      search_bar;
      controls
    }

  let save interface = 
    Html.window##scroll 0 0;
    if Composer.name interface.composer <> ""
    && Composer.kind interface.composer <> "" then begin
      Composer.submit interface.composer
        (fun set -> 
          let path_to_pdf = 
            Dancelor_router.SetPdf set
            |> Dancelor_router.path_of_controller
            |> snd
          in
          Html.window##.location##.href := js path_to_pdf)
    end else begin
      if Composer.name interface.composer = "" then 
        Widgets.Utils.add_classes interface.set_name ["has-error"];
      if Composer.kind interface.composer = "" then 
        Widgets.Utils.add_classes interface.set_kind ["has-error"];
    end

  let tune_actions interface tune_header index refresh = 
    let actions = 
      Widgets.Elements.div ~classes:["tune-actions"; "widget-bar"] 
        ~document:interface.document ~parent:tune_header ()
    in
    let button1 = 
      Widgets.Elements.button 
        ~classes:["my-btn"; "my-btn-def"; "rotate-down"; "left-widget"] 
        ~document:interface.document ~parent:actions
        ~callback:(fun () ->
          Composer.move_down interface.composer index;
          Composer.save interface.composer;
          refresh interface) ()
    in
    let button2 = 
      Widgets.Elements.button ~classes:["my-btn"; "my-btn-def"; "center-widget"]
        ~document:interface.document ~parent:actions
        ~callback:(fun () ->
          Composer.move_up interface.composer index;
          Composer.save interface.composer;
          refresh interface) ()
    in
    let button3 = 
      Widgets.Elements.button ~classes:["my-btn"; "my-btn-danger"; "right-widget"] 
        ~document:interface.document ~parent:actions
        ~callback:(fun () ->
          Composer.remove interface.composer index;
          Composer.save interface.composer;
          refresh interface) ()
    in
    Widgets.Elements.image ~classes:["btn-icon"]
      ~src:"/arrow.svg" ~parent:button1 ~document:interface.document ()
    |> ignore;
    Widgets.Elements.image ~classes:["btn-icon"]
      ~src:"/arrow.svg" ~parent:button2 ~document:interface.document ()
    |> ignore;
    Widgets.Elements.image ~classes:["btn-icon"]
      ~src:"/cross.svg" ~parent:button3 ~document:interface.document ()
    |> ignore;
    actions

  let tune_data interface tune_header tune = 
    let data = 
      Widgets.Elements.div ~classes:["tune-data"] ~document:interface.document
        ~parent:tune_header ()
    in
    let tune_group = Dancelor_model.Tune.group tune in
    let name = Dancelor_model.TuneGroup.name tune_group in
    Widgets.Elements.textnode ~text:name ~document:interface.document ~parent:data ()
    |> ignore;
    data

  let tune_area interface tune index refresh = 
    let area = 
      Widgets.Elements.div ~classes:["tune-area"] ~document:interface.document 
        ~parent:interface.tunes_area ()
    in
    let header = 
      Widgets.Elements.div ~classes:["tune-header"] ~document:interface.document ~parent:area ()
    in
    tune_data interface header tune |> ignore;
    tune_actions interface header index refresh |> ignore;
    let src = Dancelor_router.(path_of_controller (TunePng tune)) |> snd in
    Widgets.Elements.image ~document:interface.document ~parent:area
      ~classes:["tune-png"] ~src () |> ignore;
    area

  let clear interface =
    Composer.clear interface.composer;
    Composer.erase_storage interface.composer

  let rec refresh interface =
    Widgets.Utils.remove_children interface.tunes_area;
    Composer.iter interface.composer (fun index tune ->
      tune_area interface tune index refresh
      |> ignore;
    );
    if Composer.count interface.composer >= 1 then
      Controls.show interface.controls
    else
      Controls.hide interface.controls;
    interface.set_name##.value := js (Composer.name interface.composer);
    interface.set_kind##.value := js (Composer.kind interface.composer)

  let make_search_result interface tune score =
    let document = interface.document in
    let parent = Widgets.Elements.tr ~document () in
    let tune_group = Dancelor_model.Tune.group tune in
    let tune_name = Dancelor_model.TuneGroup.name tune_group in
    let tune_bars = Dancelor_model.Tune.bars tune in
    let tune_kind = Dancelor_model.TuneGroup.kind tune_group in
    let tune_structure = Dancelor_model.Tune.structure tune in
    let score_str = string_of_int score in
    let bars_kind_str =
      Printf.sprintf "%i %s"
        tune_bars (Dancelor_model.Kind.base_to_string tune_kind)
    in
    Widgets.Elements.td ~parent ~classes:["tune-info"] ~document 
      ~text:score_str () |> ignore;
    Widgets.Elements.td ~parent ~classes:["tune-info"] ~document 
      ~text:tune_name () |> ignore;
    Widgets.Elements.td ~parent ~classes:["tune-info"] ~document 
      ~text:bars_kind_str () |> ignore;
    Widgets.Elements.td ~parent ~classes:["tune-info"] ~document 
      ~text:tune_structure () |> ignore;
    parent

  let search_tunes interface input =
    let _, tune_path = Dancelor_router.path_of_controller TuneAll in
    Helpers.send_request ~path:tune_path
      ~args:["name", input; "hard-limit", "10"; "threshold", "50"]
      ~callback:(fun str ->
        let open Dancelor_common in
        Widgets.SearchBar.rem_results interface.search_bar;
        Json.from_string str
        |> Json.find ["tunes"]
        |> Json.list (Option.wrap_fun Json.of_value)
        |> Option.unwrap
        |> List.sub 10
        |> List.map (fun json ->
            (Json.find ["score"] json |> Json.int |> Option.unwrap),
            (Dancelor_model.Tune.of_json json))
        |> List.iter (fun (score, tune) ->
            let result_div = make_search_result interface tune score in
            let callback = (fun () -> 
              Composer.add interface.composer tune;
              Composer.save interface.composer;
              refresh interface) in
            Widgets.SearchBar.add_result 
             ~callback interface.search_bar result_div)) ()

  let connect interface =
    Widgets.SearchBar.on_update interface.search_bar (fun () ->
      let contents = Widgets.SearchBar.contents interface.search_bar in
      if contents <> "" then
        search_tunes interface contents);
    Controls.bind_clear interface.controls (fun () ->
      clear interface; refresh interface);
    Controls.bind_save interface.controls (fun () -> save interface);
    Lwt.async (fun () ->
      Lwt_js_events.keyups interface.set_name
        (fun _ev _ ->
          let input = Js.to_string interface.set_name##.value in
          if input <> "" then
            Widgets.Utils.rem_classes interface.set_name ["has-error"];
          Composer.set_name interface.composer input;
          Composer.save interface.composer;
          Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.keyups interface.set_kind
        (fun _ev _ ->
          let input = Js.to_string interface.set_kind##.value in
          if input <> "" then
            Widgets.Utils.rem_classes interface.set_kind ["has-error"];
          Composer.set_kind interface.composer input;
          Composer.save interface.composer;
          Lwt.return ()))

end

let on_load _ev =
  let composer = Composer.create () in
  let document = Html.window##.document in
  let content =
    Js.Opt.get (document##getElementById (js "content"))
      (fun () -> assert false)
  in
  let interface = Interface.create composer content in
  Composer.load composer (fun () -> Interface.refresh interface);
  Interface.refresh interface;
  Interface.connect interface;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
