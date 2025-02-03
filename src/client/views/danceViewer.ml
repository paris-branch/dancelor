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
    [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_dance slug)
        (Lwt.map Any.dance dance_lwt);
      h2 ~a: [a_class ["title"]] [R.txt title];
      L.h3
        ~a: [a_class ["title"]]
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
          | Some true -> [h3 ~a: [a_class ["title"]] [txt "Two Chords"]]
          | None -> [h3 ~a: [a_class ["title"]] [txt "Two Chords: unknown"]]
        );
      div
        ~a: [a_class ["buttons"]]
        [
          a
            ~a: [
              a_class ["button"];
              a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (DanceDownloadDialog.create_and_open slug)); false);
            ]
            [
              i ~a: [a_class ["material-symbols-outlined"]] [txt "picture_as_pdf"];
              txt " PDF";
            ];
        ];
      L.div
        (
          match%lwt Lwt.map Dance.date dance_lwt with
          | None -> Lwt.return_nil
          | Some date ->
            Lwt.return [txt "Devised "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      L.div
        (
          match%lwt Lwt.map Dance.scddb_id dance_lwt with
          | None -> Lwt.return_nil
          | Some scddb_id ->
            let href = Uri.to_string @@ SCDDB.dance_uri scddb_id in
            Lwt.return
              [
                txt "See on ";
                a
                  ~a: [a_href href; a_target "blank"]
                  [
                    txt "the Strathspey Database"
                  ];
                txt ".";
              ]
        );
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Recommended Tunes"];
          L.div
            (
              let%lwt tunes =
                let%lwt dance = dance_lwt in
                Tune.search' @@ Tune.Filter.existsDance' @@ Dance.Filter.is' dance
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
