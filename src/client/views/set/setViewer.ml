open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  let set_lwt = Set.get slug in

  Lwt.async (fun () ->
      let%lwt set = set_lwt in
      let%lwt title = Set.name set in
      document##.title := js (title ^ " | Set | Dancelor");
      Lwt.return ()
    );

  let open Dancelor_client_html in

  let (download_dialog, show_download_dialog) = SetDownloadDialog.create_and_render slug in

  (
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [L.txt (set_lwt >>=| Set.name)];
      L.h3 ~a:[a_class ["title"]] (set_lwt >>=| Formatters.Set.works);
      h3 ~a:[a_class ["title"]] [
        L.txt (set_lwt >>=| Set.kind >|=| Kind.Dance.to_pretty_string);
        txt " — Play ";
        L.txt (set_lwt >>=| Set.order >|=| SetOrder.to_pretty_string);
      ];
      L.h3 ~a:[a_class ["title"]] (
        match%lwt set_lwt >>=| Set.deviser with
        | None -> Lwt.return_nil
        | Some deviser ->
          let%lwt line_block = Formatters.Credit.line ~link:true (Some deviser) in
          Lwt.return (txt "Set devised by " :: line_block)
      );

      download_dialog;

      div ~a:[a_class ["buttons"]] [
        a
          ~a:[
            a_class ["button"];
            a_onclick (fun _ -> show_download_dialog (); false);
          ]
          [
            i ~a:[a_class ["fas"; "fa-file-pdf"]] [];
            txt " PDF";
          ]
      ];

      p [ L.txt (
          match%lwt set_lwt >>=| Set.instructions with
          | "" -> Lwt.return ""
          | instructions -> Lwt.return ("Instructions: " ^ instructions)) ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Previsualisation"];

        L.div (
          let%lwt set = set_lwt in
          let%lwt versions_and_parameters = Set.versions_and_parameters set in

          Lwt_list.map_p
            (fun (version, _parameters) ->
               (* FIXME: use parameters *)
               let%lwt tune = Version.tune version in
               let%lwt slug = Version.slug version in

               Lwt.return (
                 div ~a:[a_class ["image-container"]]
                   [
                     h4 [a ~a:[a_href PageRouter.(path (Version slug))] [L.txt (Tune.name tune)]];

                     object_ ~a:[
                       a_mime_type "image/svg+xml";
                       a_data ApiRouter.(path (versionSvg slug None));
                     ] [];
                   ]
               )
            )
            versions_and_parameters
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Books in Which This Set Appears"];

        L.div (
          let books_lwt =
            let%lwt set = set_lwt in
            let filter = Book.Filter.memSet set in
            Book.search filter
            >|=| Score.list_erase
          in
          let%lwt books = books_lwt in

          Lwt.return [
            if books = [] then
              p [txt "There are no books containing this set."]
            else
              Dancelor_client_tables.books books
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
