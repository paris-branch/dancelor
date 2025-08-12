open Nes
open Common

open Model
open Html

let create ?context id =
  MainPage.madge_call_or_404 (Tune Get) id @@ fun tune ->
  Page.make'
    ~parent_title: "Tune"
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_tune id)
        (lwt @@ Any.tune tune);
    ]
    ~title: (lwt @@ Tune.one_name' tune)
    ~subtitles: [
      Formatters.Tune.aka' tune;
      Formatters.Tune.description' tune;
    ]
    [
      div
        (
          match Tune.date' tune with
          | None -> []
          | Some date -> [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              li
                (
                  match Tune.scddb_id' tune with
                  | None -> []
                  | Some scddb_id ->
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
        (lwt tune)
        [
          ("sets containing this tune", Filter.(Any.set' % Set.existsVersion' % Version.tuneIs'));
          ("books containing this tune", Filter.(Any.book' % Book.memTuneDeep'));
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Versions of this tune"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt versions =
                  snd
                  <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) Slice.everything @@
                      Filter.Version.tuneIs' tune
                in
                lwt @@
                  if versions = [] then
                      [txt "There are no versions for this tune."]
                  else
                      [Tables.versions versions]
            )
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Dances that recommend this tune"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt dances = Tune.dances' tune in
                lwt
                  [
                    if dances = [] then
                      txt "There are no dances that recommend this tune."
                    else
                      Tables.dances dances
                  ]
            )
        ];
    ]
