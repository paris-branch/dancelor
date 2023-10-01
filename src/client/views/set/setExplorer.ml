open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    content : Html.divElement Js.t;
  }

let create page =
  let document = Page.document page in
  let content = Html.createDiv document in

  document##.title := js "All Sets | Dancelor";

  let pagination =
    PageNavNewAPI.create
      (* ~entries: (Set.count Formula.true_) *)
      ~entries_per_page: 25
  in

  (
    let open Dancelor_client_html.NewAPI in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "All sets"];

      tablex
        ~a:[a_class ["FIXME-separated-table"]]
        ~thead:(
          thead [
            tr [
              th [txt "Name"]; (* FIXME: alt text “Sets” when collapsed *)
              th [txt "Deviser"];
              th [txt "Kind"];
              th [txt "Actions"];
            ]
          ]
        )
        [
          R.tbody (
            S.bind pagination.signal @@ fun _pagination ->
            S.from' [] @@ Lwt.map
              (List.map
                 (fun set ->
                    let href =
                      let%lwt slug = Set.slug set in
                      Lwt.return PageRouter.(path (Set slug))
                    in
                    let open Lwt in
                    Dancelor_client_tables.TheNewAPI.clickable_row ~href [
                      (Formatters.SetNewAPI.name_and_tunes ~link:false set);
                      (Set.deviser set >>= Formatters.CreditNewAPI.line);
                      Lwt.return [L.txt (Set.kind set >|= Kind.Dance.to_string)];
                      Lwt.return [txt ""];
                    ]
                 ))
              (* (Set.search ~pagination Formula.true_ >|=| Score.list_erase) *)
              (Set.search Formula.true_ >|=| Score.list_erase)
          )
        ];

      PageNavNewAPI.render pagination;
    ]
  );

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
