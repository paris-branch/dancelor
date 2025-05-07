open Nes
open Common

open Model
open Html

let create ?context slug =
  let source_lwt = MainPage.get_model_or_404 (Source Get) slug in
  let title = S.from' "" (Lwt.map Source.name' source_lwt) in
  Page.make
    ~parent_title: "Source"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_source slug)
        (Lwt.map Any.source source_lwt);
    ]
    [
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              L.li
                (
                  match%lwt Lwt.map Source.scddb_id' source_lwt with
                  | None -> Lwt.return_nil
                  | Some scddb_id ->
                    Lwt.return
                      [
                        a
                          ~a: [
                            a_class ["dropdown-item"];
                            a_href (Uri.to_string @@ SCDDB.publication_uri scddb_id);
                          ]
                          [
                            i ~a: [a_class ["bi"; "bi-box-arrow-up-right"]] [];
                            txt " See on SCDDB";
                          ]
                      ]
                );
            ];
        ];
      div
        ~a: [a_class ["d-flex"; "flex-column"; "flex-sm-row"; "mt-2"]]
        [
          div
            ~a: [a_class ["flex-shrink-1"]]
            [
              img ~a: [a_style "max-width: 100%;"] ~alt: "Cover" ~src: (Endpoints.Api.(href @@ Source Cover) slug) ()
            ];
          div
            ~a: [a_class ["mt-2"; "mt-sm-0"; "ms-sm-2"]]
            [
              p
                [
                  L.txt
                    (
                      match%lwt Lwt.map Source.description' source_lwt with
                      | None -> Lwt.return "no description available"
                      | Some description -> Lwt.return description
                    )
                ];
            ];
        ];
      div
        ~a: [a_class ["mt-2"; "mt-sm-0"; "ms-sm-2"]]
        [
          p
            [
              L.txt
                (
                  match%lwt Lwt.map Source.description' source_lwt with
                  | None -> Lwt.return "no description available"
                  | Some description -> Lwt.return description
                )
            ];
          Utils.quick_explorer_links
            [
              ("versions from this source", Lwt.map (Any.Filter.version' % Version.Filter.memSource') source_lwt);
            ];
        ];
    ]
