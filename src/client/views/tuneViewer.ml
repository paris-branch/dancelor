open Nes
open Common

open Model
open Html

let create ?context slug =
  let tune_lwt = Tune.get slug in
  let title = S.from' "" (Lwt.map Tune.name tune_lwt) in
  Page.make
    ~parent_title: "Tune"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_tune slug)
        (Lwt.map Any.tune tune_lwt);
    ]
    [
      L.h5 ~a: [a_class ["text-center"]] (Lwt.map Formatters.Tune.aka tune_lwt);
      L.h5 ~a: [a_class ["text-center"]] (tune_lwt >>=| Formatters.Tune.description);
      L.div
        (
          match%lwt Lwt.map Tune.date tune_lwt with
          | None -> Lwt.return_nil
          | Some date ->
            Lwt.return [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              L.li
                (
                  match%lwt Lwt.map Tune.scddb_id tune_lwt with
                  | None -> Lwt.return_nil
                  | Some scddb_id ->
                    Lwt.return
                      [
                        a
                          ~a: [
                            a_class ["dropdown-item"];
                            a_href (Uri.to_string @@ SCDDB.tune_uri scddb_id);
                          ]
                          [
                            i ~a: [a_class ["bi"; "bi-box-arrow-up-right"]] [];
                            txt " See on SCDDB";
                          ]
                      ]
                );
            ];
        ];
      Utils.quick_explorer_links'
        tune_lwt
        [
          ("sets containing this tune", Any.Filter.set' % Set.Filter.existsVersion' % Version.Filter.tuneIs');
          ("books containing this tune", Any.Filter.book' % Book.Filter.memTuneDeep');
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Versions of this tune"];
          L.div
            (
              let%lwt tune = tune_lwt in
              let%lwt versions =
                Lwt.map snd @@
                Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Version Search) Slice.everything @@
                Version.Filter.tuneIs' tune
              in
              Lwt.return @@
                if versions = [] then
                  [
                    txt "There are no versions for this tune. Maybe you want to ";
                    a ~a: [a_href (Endpoints.Page.href_versionAdd ~tune: (Entry.slug tune) ())] [txt "add one"];
                    txt "?";
                  ]
                else
                    [Tables.versions versions]
            )
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Dances that recommend this tune"];
          L.div
            (
              let%lwt tune = tune_lwt in
              let%lwt dances = Tune.dances tune in
              Lwt.return
                [
                  if dances = [] then
                    txt "There are no dances that recommend this tune."
                  else
                    Tables.dances dances
                ]
            )
        ];
    ]
