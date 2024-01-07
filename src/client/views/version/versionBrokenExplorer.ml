open Nes
open Js_of_ocaml
open Dancelor_client_model

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let create page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in

  document##.title := js "Broken versions | Dancelor";

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "List of broken versions"];

      L.div (
        let%lwt versions = Version.search' Version.Filter.broken >|=| Score.list_erase in

        (* If there is no broken version, no need to print the table *)
        if List.length versions = 0 then
          Lwt.return [txt "No broken version"]
        else
          Lwt.return [Dancelor_client_tables.versions_with_names versions]
      )
    ]
  );

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
