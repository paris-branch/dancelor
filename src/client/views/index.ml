open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_utils
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    document : Html.document Js.t;
    content : Html.divElement Js.t;
  }

let search search_text =
  let threshold = 0.4 in
  let pagination = Pagination.{ start = 0; end_ = 15 } in
  let%rlwt filter = Lwt.return (Any.Filter.from_string search_text) in (* FIXME: AnyFilter.from_string should return a result lwt *)
  let%lwt results = Any.search ~threshold ~pagination filter in
  Lwt.return_ok results

let create page =
  let document = Html.window##.document in
  let content = Html.createDiv document in

  let search_text = Lwt_bchan.create "" in

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      div const [

        (* FIXME: on_focus: make the table visible *)
        (* FIXME: on_enter: redirect to /search *)
        input
          ~type_:Text
          ~placeholder:"Search for anything; it's magic! (but currently broken)"
          ~on_input:(Lwt_bchan.put search_text)
          ();

        table ~classes:["dropdown-table"; "visible"] loop (
          let search_text = Lwt_bchan.create_port search_text in
          fun () ->
            let%lwt search_text = Lwt_bchan.take search_text in
            if String.length search_text < 3 then
              (
                let help_message =
                  if search_text = "" then "Start typing to search."
                  else "Type at least three characters."
                in
                Lwt.return [
                  tr const [
                    td const [text const "👉"];
                    td const [text const help_message];
                  ]
                ]
              )
            else
              (
                match%lwt search search_text with

                | Ok [] ->
                  Lwt.return [
                    tr const [
                      td const [text const "⚠️"];
                      td const [text const "Your search returned no results."];
                    ];
                  ]

                | Ok results ->
                  let%lwt lines = Lwt_list.map_s AnyResultHtml.make_result results in
                  Lwt.return (
                    lines @ [
                      tr const [
                        td const [text const "👉"];
                        td const [text const "Press enter for more results."];
                      ]
                    ]
                  )

                | Error messages ->
                  Lwt.return (
                    List.map
                      (fun message ->
                         tr const [
                           td const [text const "❌"];
                           td const [text const message];
                         ])
                      messages
                  )
              )
        )
      ]
    ]);

  {page; document; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init _t =
  ()
