open Nes
open Common

open Model
open Html

let create ?context slug =
  MainPage.get_model_or_404 (Dance Get) slug @@ fun dance ->
  let title = S.const (Dance.name' dance) in
  Page.make
    ~parent_title: "Dance"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_dance slug)
        (Lwt.return @@ Any.dance dance);
    ]
    [
      h5
        ~a: [a_class ["text-center"]]
        [
          L.inline_placeholder @@
            let kind = [txt @@ Kind.Dance.to_pretty_string @@ Dance.kind' dance] in
            let%lwt by =
              match%lwt Dance.devisers' dance with
              | [] -> Lwt.return_nil
              | devisers -> Lwt.return [txt " by "; Formatters.Person.names' ~links: true devisers]
            in
            Lwt.return (kind @ by)
        ];
      div
        (
          match Dance.two_chords' dance with
          | Some false -> []
          | Some true -> [h5 ~a: [a_class ["text-center"]] [txt "Two Chords"]]
          | None -> [h5 ~a: [a_class ["text-center"]] [txt "Two Chords: unknown"]]
        );
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              li
                [
                  a
                    ~a: [
                      a_class ["dropdown-item"];
                      a_href "#";
                      a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (DanceDownloadDialog.create_and_open slug)); false);
                    ]
                    [
                      i ~a: [a_class ["bi"; "bi-file-pdf"]] [];
                      txt " Download PDF";
                    ];
                ];
              li
                (
                  match Dance.scddb_id' dance with
                  | None -> []
                  | Some scddb_id ->
                    [
                      a
                        ~a: [
                          a_class ["dropdown-item"];
                          a_href (Uri.to_string @@ SCDDB.dance_uri scddb_id);
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
        (
          match Dance.date' dance with
          | None -> []
          | Some date -> [txt "Devised "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      div
        [
          h3 [txt "Recommended Tunes"];
          L.div
            (
              let%lwt tunes =
                Lwt.map snd @@
                Madge_client.call_exn
                  Endpoints.Api.(route @@ Tune Search)
                  Slice.everything @@
                Filter.Tune.existsDance' @@ Filter.Dance.is' dance
              in
              Lwt.return
                [
                  if tunes = [] then
                    txt
                      (
                        "There are no recommended tunes for this dance. " ^
                        "Dancelor is not all-knowing: go check the Strathspey Database! " ^
                        "And if you find something that is not known here, report it to someone."
                      )
                  else
                    Tables.tunes tunes
                ]
            )
        ];
    ]
