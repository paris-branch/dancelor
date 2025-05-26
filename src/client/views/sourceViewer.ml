open Nes
open Common

open Model
open Html

let create ?context slug =
  MainPage.get_model_or_404 (Source Get) slug @@ fun source ->
  let title = S.const (Source.name' source) in
  Page.make
    ~parent_title: "Source"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        (* FIXME: doesn't need to take an [Lwt.t] anymore? *)
        ?context
        ~this_page: (Endpoints.Page.href_source slug)
        (Lwt.return @@ Any.source source);
    ]
    [
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              li
                (
                  match Source.scddb_id' source with
                  | None -> []
                  | Some scddb_id ->
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
                  txt
                    (
                      match Source.description' source with
                      | None -> "no description available"
                      | Some description -> description
                    )
                ];
            ];
        ];
      Utils.quick_explorer_links
        [
          ("versions from this source", Lwt.return @@ Filter.(Any.version' % Version.memSource') source);
        ];
    ]
