open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Interface = struct

  type t = {
    document : Html.document Js.t;
    table : Html.tableElement Js.t;
    mutable current_dropdown : Html.uListElement Js.t option;
  }

  let create parent =
    let document = Html.window##.document in
    let title = Html.createH1 document in
    title##.textContent := Js.some (js "All Sets");
    Dom.appendChild parent title;
    let table = Widgets.Elements.table ~document ~parent
      ~classes:["separated-table"] ()
    in
    {document; table; current_dropdown = None}

  let add_table_header interface =
    let document = interface.document in
    let parent =
      Widgets.Elements.tr ~parent:interface.table
        ~classes:["table-header"] ~document:document ()
    in
    Widgets.Elements.td ~parent ~document ~text:"Name" () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:"Deviser" () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:"Kind" () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:"Actions" () |> ignore

  let add_entry interface set =
    let document = interface.document in
    let name = Dancelor_model.Set.name set in
    let kind =
      Dancelor_model.Set.kind set
      |> Dancelor_model.Kind.dance_to_string
    in
    let deviser =
      match Dancelor_model.Set.deviser set with
      | None -> ""
      | Some cr ->
        (* FIXME: this requires an API call. *)
        Dancelor_model.Credit.line cr
    in
    let parent =
      Widgets.Elements.tr ~parent:interface.table ~classes:["set-info"]
        ~document:interface.document ()
    in
    let td_name = Widgets.Elements.td ~parent ~document () in
    let _, set_path = Dancelor_router.path_of_controller (Set (Dancelor_model.Set.slug set)) in
    Widgets.Elements.a ~classes:["my-link"] ~parent:td_name ~document
      ~text:name ~href:set_path () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:deviser () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:kind () |> ignore;
    let options = Widgets.Elements.td ~parent ~document () in
    let download =
      Widgets.Elements.image ~classes:["icon"] ~parent:options
        ~src:"/download.svg" ~document ()
    in
    let edit =
      Widgets.Elements.image ~classes:["icon"] ~parent:options
        ~src:"/edit.svg" ~document ()
    in
    let delete =
      Widgets.Elements.image ~classes:["icon"] ~parent:options
        ~src:"/cross_red_circle.svg" ~document ()
    in
    Lwt.async (fun () ->
      Lwt_js_events.clicks download
        (fun ev _ ->
          Dom_html.stopPropagation ev;
          let dwn_c = Widgets.Elements.li ~document ~text:"Download C" () in
          let dwn_b = Widgets.Elements.li ~document ~text:"Download B flat" () in
          let dwn_e = Widgets.Elements.li ~document ~text:"Download E flat" () in
          let dwn_ly = Widgets.Elements.li ~document ~text:"Download .ly" () in
          let dropdown =
            Widgets.Elements.dropdown
               ~entries:[
                 dwn_c, (fun () ->
                     let _, path = Dancelor_router.path_of_controller (SetPdf (Dancelor_model.Set.slug set)) in
                     Html.window##.location##.href := js path);
                 dwn_b, (fun () ->
                     let _, path = Dancelor_router.path_of_controller (SetPdf (Dancelor_model.Set.slug set)) in
                     let path = Printf.sprintf "%s?transpose-target=bes%%2c" path in
                     Html.window##.location##.href := js path);
                 dwn_e, (fun () ->
                     let _, path = Dancelor_router.path_of_controller (SetPdf (Dancelor_model.Set.slug set)) in
                     let path = Printf.sprintf "%s?transpose-target=ees" path in
                     Html.window##.location##.href := js path);
                 dwn_ly, (fun () ->
                     let _, path = Dancelor_router.path_of_controller (SetLy (Dancelor_model.Set.slug set)) in
                     Html.window##.location##.href := js path)]
               ~parent:options
               ~document ()
          in
          begin match interface.current_dropdown with
          | None -> ()
          | Some menu -> Widgets.Utils.destroy menu
          end;
          interface.current_dropdown <- Some dropdown;
          Lwt.return ()
        ));
    Lwt.async (fun () ->
      Lwt_js_events.clicks edit
        (fun ev _ ->
          if interface.current_dropdown = None then begin
            Dom_html.stopPropagation ev;
            Html.window##alert (js "You cannot edit sets yet, but the button is beautiful right? Right?!?");
          end; Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.clicks delete
        (fun ev _ ->
          if interface.current_dropdown = None then begin
            Dom_html.stopPropagation ev;
            let msg = Printf.sprintf "Do you really want to delete %s?" name in
            let answer = Html.window##confirm (js msg) in
            if Js.to_bool answer then begin
              let _, path = Dancelor_router.path_of_controller (SetDelete (Dancelor_model.Set.slug set)) in
              Helpers.send_request
                ~meth:"DELETE"
                ~callback:(fun _ ->
                  Dom.removeChild interface.table parent |> ignore)
                ~path ()
            end
          end; Lwt.return ()));
    Lwt.async (fun () ->
      Lwt_js_events.clicks parent
        (fun _ev _ ->
          if interface.current_dropdown = None then begin
            let _, path = Dancelor_router.path_of_controller (Set (Dancelor_model.Set.slug set)) in
            Html.window##.location##.href := js path;
          end; Lwt.return ()))

  let fill_table interface sets =
    add_table_header interface;
    List.iter (add_entry interface) sets

  let connect interface =
    let _, path = Dancelor_router.path_of_controller SetAll in
    Helpers.send_request ~path
      ~callback:(fun str ->
        let open Dancelor_common in
        Json.from_string str
        |> Json.find ["sets"]
        |> Json.list Json.of_value
        |> Option.unwrap
        |> List.map Dancelor_model.Set.of_json
        |> fill_table interface
      ) ();
    Lwt.async (fun () ->
      Lwt_js_events.clicks interface.document
        (fun _ _ ->
          begin match interface.current_dropdown with
          | None -> ()
          | Some menu -> Widgets.Utils.destroy menu
          end;
          interface.current_dropdown <- None;
          Lwt.return ()))

end

let on_load _ev =
  let document = Html.window##.document in
  let content =
    Js.Opt.get (document##getElementById (js "content"))
      (fun () -> assert false)
  in
  let interface = Interface.create content in
  Interface.connect interface;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
