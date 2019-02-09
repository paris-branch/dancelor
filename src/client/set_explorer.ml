open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Interface = struct

  type t = {
    document : Html.document Js.t;
    table : Html.tableElement Js.t;
  }
  
  let create parent = 
    let document = Html.window##.document in
    let title = Html.createH1 document in
    title##.textContent := Js.some (js "All Sets");
    Dom.appendChild parent title;
    let table = Widgets.Elements.table ~document ~parent 
      ~classes:["separated-table"] () 
    in
    {document; table}

  let add_table_header interface = 
    let document = interface.document in
    let parent = 
      Widgets.Elements.tr ~parent:interface.table 
        ~classes:["table-header"] ~document:document () 
    in
    Widgets.Elements.td ~parent ~document ~text:"Name" () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:"Deviser" () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:"Kind" () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:"Links" () |> ignore

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
      | Some cr -> Dancelor_model.Credit.line cr
    in
    let parent = 
      Widgets.Elements.tr ~parent:interface.table ~classes:["set-info"]
        ~document:interface.document () 
    in
    Widgets.Elements.td ~parent ~document ~text:name () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:deviser () |> ignore;
    Widgets.Elements.td ~parent ~document ~text:kind () |> ignore;
    Widgets.Elements.td ~parent ~document () |> ignore;
    Lwt.async (fun () ->
      Lwt_js_events.clicks parent
        (fun _ev _ -> 
          let _, path = Dancelor_router.path_of_controller (Set set) in
          Html.window##.location##.href := js path; Lwt.return ()))

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
        |> Json.list (Option.wrap_fun Json.of_value)
        |> Option.unwrap
        |> List.map Dancelor_model.Set.of_json
        |> fill_table interface
      ) ()

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
