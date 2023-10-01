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

  document##.title := js "All sets | Dancelor";

  let pagination =
    PageNavNewAPI.create
      ~number_of_entries: (Set.count Formula.true_)
      ~entries_per_page: 25
  in

  (
    let open Dancelor_client_html.NewAPI in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "All sets"];

      PageNavNewAPI.render pagination;

      tablex
        ~a:[a_class ["separated-table"]]
        ~thead:(
          thead [
            tr [
              th [
                span ~a:[a_class ["full-content"]] [txt "Name"];
                span ~a:[a_class ["collapse-content"]] [txt "Sets"];
              ];
              th [txt "Deviser"];
              th [txt "Kind"];
              th [txt "Actions"];
            ]
          ]
        )
        [
          R.tbody (
            S.bind pagination.signal @@ fun pagination ->
            S.from' [] @@
            Fun.flip Lwt.map
              (Set.search ~pagination:(PageNavNewAPI.current_pagination pagination) Formula.true_ >|=| Score.list_erase)
              (List.map
                 (fun set ->
                    let href =
                      let%lwt slug = Set.slug set in
                      Lwt.return PageRouter.(path (Set slug))
                    in
                    let open Lwt in
                    Dancelor_client_tables.clickable_row ~href [
                      (Formatters.SetNewAPI.name_and_tunes ~link:false set);
                      (Set.deviser set >>= Formatters.CreditNewAPI.line);
                      Lwt.return [L.txt (Set.kind set >|= Kind.Dance.to_string)];
                      Lwt.return [txt ""];
                    ]
                 )
              )
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
