open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

let js = Js.string

type t =
  {
    page : Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let version_lwt = Version.get slug in
  let tune_lwt = version_lwt >>=| Version.tune in

  Lwt.async (fun () ->
      let%lwt tune = tune_lwt in
      let%lwt name = Tune.name tune in
      document##.title := js (name ^ " | Tune | Dancelor");
      Lwt.return ()
    );

  let other_versions_lwt =
    let%lwt tune = tune_lwt in
    let%lwt version = version_lwt in
    let filter =
      Formula.(and_l [
          Version.Filter.tuneIs tune;
          not_ (Version.Filter.is version);
        ])
    in
    Version.search filter
    >|=| Score.list_erase
  in

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] const [text lwt (tune_lwt >>=| Tune.name)];
      h3 ~classes:["title"] lwt (tune_lwt >>=| Formatters.Tune.aka);
      h3 ~classes:["title"] lwt (tune_lwt >>=| Formatters.Tune.description);
      h3 ~classes:["title"] lwt (version_lwt >>=| Formatters.Version.description ~link:true);
      div lwt (
        match%lwt tune_lwt >>=| Tune.scddb_id with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = const @@ SCDDB.tune_uri scddb_id in
          Lwt.return [
            h3 ~classes:["title"] const [
              a ~href ~target:Blank const [
                text const "Link to the Strathspey Database"
              ]
            ]
          ]
      );

      div ~classes:["buttons"] const (
        let bass_parameters =
          VersionParameters.(
            make ~clef:Music.Bass
              ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1)))
              ()
          )
        in
        let b_pitch = Music.make_pitch B Flat (-1) in
        let b_parameters = VersionParameters.(
            make ~transposition:(Transposition.relative b_pitch Music.pitch_c)
              ()
          )
        in
        let e_pitch = Music.make_pitch E Flat 0 in
        let e_parameters = VersionParameters.(
            make ~transposition:(Transposition.relative e_pitch Music.pitch_c)
              ()
          )
        in

        let c_pdf_href, b_pdf_href, e_pdf_href, bass_pdf_href, ly_href =
          const @@ ApiRouter.(path @@ versionPdf slug @@ Option.none),
          const @@ ApiRouter.(path @@ versionPdf slug @@ Option.some    b_parameters),
          const @@ ApiRouter.(path @@ versionPdf slug @@ Option.some    e_parameters),
          const @@ ApiRouter.(path @@ versionPdf slug @@ Option.some bass_parameters),
          const @@ ApiRouter.(path @@ versionLy slug)
        in

        let pdf_button href txt =
          a ~classes:["button"] ~href ~target:Blank const [
            i ~classes:["fas"; "fa-file-pdf"] const [];
            text const (" " ^ txt)
          ]
        in

        let add_to_set_button =
          Inputs.Button.create
            ~on_click:(fun () ->
                SetEditor.add_to_storage slug;
                let href = PageRouter.(path SetCompose) in
                Dom_html.window##.location##.href := js href)
            ~text:("Add to current set") page
        in
        [
          pdf_button c_pdf_href    "PDF";
          pdf_button b_pdf_href    "PDF (B♭)";
          pdf_button e_pdf_href    "PDF (E♭)";
          pdf_button bass_pdf_href "PDF (𝄢)";
          br;
          a ~classes:["button"] ~href:ly_href const [
            i ~classes:["fas"; "fa-file-alt"] const [];
            text const " LilyPond"
          ];
          br;
          node_of_dom_node (Inputs.Button.root add_to_set_button :> dom_node)
        ]
      );

      div ~classes:["section"] const [
        h3 const [text const "Previsualisation"];

        div ~classes:["image-container"] const [
          object_
            ~type_:"image/svg+xml"
            ~data:ApiRouter.(path (versionSvg slug None))
            const
            [];
        ]
      ];

      div ~classes:["audio-container"] const [
        audio ~src:ApiRouter.(path (versionOgg slug)) ~controls:true ()
      ];

      div ~classes:["buttons"] lwt (
        let%lwt is_broken = version_lwt >>=| Version.broken in

        let broken =
          Inputs.Button.create
            ~on_click:(fun () -> Lwt.async (fun () ->
                if is_broken then version_lwt >>=| Version.mark_fixed else version_lwt >>=| Version.mark_broken;%lwt
                Dom_html.window##.location##reload ;
                Lwt.return_unit ))
            ~text:(if is_broken then "Mark fixed" else "Mark broken") page
        in

        Lwt.return [ node_of_dom_node (Inputs.Button.root broken :> dom_node) ]
      );

      div ~classes:["section"] const [
        h3 const [text const "Other Versions"];

        div lwt (
          let%lwt other_versions = other_versions_lwt in

          Lwt.return (
            if other_versions = [] then
              [p const [text const "There are no other versions available for this tune."]]
            else
              [
                Dancelor_client_tables.versions other_versions;

                let href =
                  lwt @@
                  let%lwt tune = tune_lwt in
                  let%lwt slug = Tune.slug tune in
                  Lwt.return PageRouter.(path (Tune slug))
                in
                p const [
                  text const "You can also go to the ";
                  a ~href const [text const "page of the tune"];
                  text const "."
                ]
              ]
          )
        );
      ];

      div ~classes:["section"] const [
        h3 const [text const "Dances That Recommend This Tune"];

        div lwt (
          let none = (Page.document page)##createTextNode (js "") in
          let none_maybe = Dom_html.createP (Page.document page) in
          Dom.appendChild none_maybe none;
          Dom.appendChild content none_maybe;

          let%lwt tune = tune_lwt in
          let%lwt dances = Tune.dances tune in

          Lwt.return [
            if dances = [] then
              text const "There are no dances that recommend this tune."
            else
              Dancelor_client_tables.dances dances
          ]
        )
      ];

      div ~classes:["section"] const [
        h3 const [text const "Sets in Which This Version Appears"];

        div lwt (
          let sets_lwt =
            let%lwt version = version_lwt in
            let filter = Set.Filter.memVersion version in
            Set.search filter
            >|=| Score.list_erase
          in
          let%lwt sets = sets_lwt in

          Lwt.return [
            if sets = [] then
              text const "There are no sets containing this version."
            else
              Dancelor_client_tables.sets sets
          ]
        );

        div lwt (
          match%lwt other_versions_lwt with
          | [] -> Lwt.return_nil
          | _ -> Lwt.return [
              let href =
                lwt @@
                let%lwt tune = tune_lwt in
                let%lwt slug = Tune.slug tune in
                Lwt.return PageRouter.(path (Tune slug))
              in
              p const [
                text const "If you want to see the sets in which this version or ";
                text const "any other appear, go to the ";
                a ~href const [text const "page of the tune"];
                text const "."
              ]
            ]
        )
      ];

      div ~classes:["section"] const [
        h3 const [text const "Books in Which This Version Appears"];

        div lwt (
          let%lwt books =
            let%lwt version = version_lwt in
            let filter = Book.Filter.memVersionDeep version in
            Book.search filter
            >|=| Score.list_erase
          in

          Lwt.return [
            if books = [] then
              text const "There are no books containing this version."
            else
              Dancelor_client_tables.books books
          ]
        );

        div lwt (
          match%lwt other_versions_lwt with
          | [] -> Lwt.return_nil
          | _ -> Lwt.return [
              let href =
                lwt @@
                let%lwt tune = tune_lwt in
                let%lwt slug = Tune.slug tune in
                Lwt.return PageRouter.(path (Tune slug))
              in
              p const [
                text const "If you want to see the books in which this version or ";
                text const "any other appear, go to the ";
                a ~href const [text const "page of the tune" ];
                text const "."
              ]
            ]
        );
      ];
    ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
