open Dancelor_common
open Js_of_ocaml
module Html = Dom_html

let on_load _ev =
  let document = Html.window##.document in
  let content =
    Js.Opt.get
      (document##getElementById (Js.string "content"))
      (fun () -> assert false)
  in
  let title = Html.createH2 document in
  Dom.appendChild content title;

  let href =
    Js.to_string Html.window##.location##.href
    |> Uri.of_string
    |> Uri.path_and_query
  in
  print_endline href;

  let person =
    match Dancelor_router.path_to_controller ~meth:`GET ~path:href with
    | Some (Dancelor_router.Person person) -> person
    | _ -> assert false
  in

  let _, path = Dancelor_router.(path_of_controller (Person person)) in
  Helpers.send_request ~path (fun str ->
      let person =
        let open Dancelor_common.Json in
        from_string str
        |> find ["person"]
        |> of_value
        |> Dancelor_model.Person.of_json
      in
      title##.textContent := Js.some (Js.string (Dancelor_model.Person.name person))
    );
  Js._false

let () =
  Html.window##.onload := Html.handler on_load
