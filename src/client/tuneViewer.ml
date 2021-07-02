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

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let tune_lwt = Tune.get slug in

  Dancelor_client_elements.H.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] [ text_lwt (tune_lwt >>=| Tune.name) ];
      h3 ~classes:["title"] [ text_lwt (Formatters.Tune.aka_lwt tune_lwt) ];
      h3 ~classes:["title"] [ text_lwt (Formatters.Tune.recommended_lwt tune_lwt) ];
      h3_lwt ~classes:["title"] (
        let%lwt tune = tune_lwt in
        let%lwt dom_nodes = Formatters.Tune.description tune page in
        Lwt.return (List.map node_of_dom_node dom_nodes)
      );

      div ~classes:["section"] [
        h3 [ text "Versions of This Tune" ];

        div_lwt (
          let versions_lwt =
            let%lwt filter =
              let%lwt tune = tune_lwt in
              Lwt.return (Version.Filter.tuneIs tune)
            in
            Version.all ~filter ()
          in
          let%lwt versions = versions_lwt in

          (* If there is only one version, redirect directly to it. *)
          if List.length versions = 1 then
            (
              Lwt.async @@ fun () ->
              let%lwt href =
                let%lwt slug = Version.slug (List.hd versions) in
                Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
              in
              Html.window##.location##.href := js href;
              Lwt.return_unit
            );

          Lwt.return [
            node_of_dom_node
              (Table.root (Dancelor_client_tables.Version.make versions_lwt page)
               :> dom_node)
          ]
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Sets in Which This Tune Appears" ];

        div_lwt (
          let none = (Page.document page)##createTextNode (js "") in
          let none_maybe = Html.createP (Page.document page) in
          Dom.appendChild none_maybe none;
          Dom.appendChild content none_maybe;

          let sets_lwt =
            let%lwt tune = tune_lwt in
            let filter = Set.Filter.existsVersion (Version.Filter.tuneIs tune) in
            Set.all ~filter ()
          in
          let%lwt sets = sets_lwt in

          Lwt.return [
            if sets = [] then
              text "There are no sets containing this tune."
            else
              node_of_dom_node
                (Table.root (Dancelor_client_tables.Set.make sets_lwt page)
                 :> dom_node)
          ]
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Books in Which This Tune Appears" ];

        div_lwt (
          let books_lwt =
            let%lwt tune = tune_lwt in
            let filter = Book.Filter.memTuneDeep tune in
            Book.all ~filter ()
          in
          let%lwt books = books_lwt in

          Lwt.return [
            if books = [] then
              text "There are no books containing this tune."
            else
              node_of_dom_node
                (Table.root (Dancelor_client_tables.Book.make books_lwt page)
                 :> dom_node)
          ]
        )
      ]
    ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
