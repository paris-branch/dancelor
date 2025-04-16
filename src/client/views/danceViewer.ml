open Nes
open Common

open Model
open Html

let create ?context slug =
  let dance_lwt = Dance.get slug in
  let title = S.from' "" (Lwt.map Dance.name dance_lwt) in
  Page.make
    ~parent_title: "Dance"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_dance slug)
        (Lwt.map Any.dance dance_lwt);
    ]
    [
      L.h5
        ~a: [a_class ["text-center"]]
        (
          let kind = [L.txt @@ Lwt.map (Kind.Dance.to_pretty_string % Dance.kind) dance_lwt] in
          let%lwt by =
            match%lwt dance_lwt >>=| Dance.devisers with
            | [] -> Lwt.return_nil
            | devisers -> Lwt.return (txt " by " :: Formatters.Person.names ~link: true devisers)
          in
          Lwt.return (kind @ by)
        );
      L.div
        (
          Fun.flip Lwt.map dance_lwt @@ fun dance ->
          match Dance.two_chords dance with
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
              L.li
                (
                  match%lwt Lwt.map Dance.scddb_id dance_lwt with
                  | None -> Lwt.return_nil
                  | Some scddb_id ->
                    Lwt.return
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
      L.div
        (
          match%lwt Lwt.map Dance.date dance_lwt with
          | None -> Lwt.return_nil
          | Some date ->
            Lwt.return [txt "Devised "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      div
        [
          h3 [txt "Recommended Tunes"];
          L.div
            (
              let%lwt tunes =
                let%lwt dance = dance_lwt in
                Lwt.map snd @@
                Madge_cohttp_lwt_client.call
                  Endpoints.Api.(route @@ Tune Search)
                  Slice.everything @@
                Tune.Filter.existsDance' @@ Dance.Filter.is' dance
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
