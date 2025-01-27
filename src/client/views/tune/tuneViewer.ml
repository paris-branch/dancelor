open Nes
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Components = Dancelor_client_components
module Page = Dancelor_client_page
module Utils = Dancelor_client_utils
open Dancelor_client_html

let create ?context slug =
  let tune_lwt = Tune.get slug in
  let title = S.from' "" (Lwt.map Tune.name tune_lwt) in
  Page.make ~title: (Page.sub_title "Tune" title) @@
  div
    [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (PageRouter.href_tune slug)
        (Lwt.map Any.tune tune_lwt);
      h2 ~a: [a_class ["title"]] [R.txt title];
      L.h3 ~a: [a_class ["title"]] (Lwt.map Formatters.Tune.aka tune_lwt);
      L.h3 ~a: [a_class ["title"]] (tune_lwt >>=| Formatters.Tune.description);
      L.div
        (
          match%lwt Lwt.map Tune.date tune_lwt with
          | None -> Lwt.return_nil
          | Some date ->
            Lwt.return [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      L.div
        (
          match%lwt Lwt.map Tune.scddb_id tune_lwt with
          | None -> Lwt.return_nil
          | Some scddb_id ->
            let href = SCDDB.tune_uri scddb_id in
            Lwt.return
              [
                txt "See on ";
                a
                  ~a: [a_href (Uri.to_string href); a_target "blank"]
                  [
                    txt "the Strathspey Database"
                  ];
                txt ".";
              ]
        );
      Utils.quick_explorer_links'
        tune_lwt
        [
          ("sets containing this tune", Any.Filter.set' % Set.Filter.existsVersion' % Version.Filter.tuneIs');
          ("books containing this tune", Any.Filter.book' % Book.Filter.memTuneDeep');
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Versions of This Tune"];
          L.div
            (
              let%lwt tune = tune_lwt in
              let%lwt versions =
                Version.search' @@ Version.Filter.tuneIs' tune
              in
              Lwt.return @@
              if versions = [] then
                [
                  txt "There are no versions for this tune. Maybe you want to ";
                  a ~a: [a_href (PageRouter.href_versionAdd ~tune: (Database.Entry.slug tune) ())] [txt "add one"];
                  txt "?";
                ]
              else
                [Dancelor_client_tables.versions versions]
            )
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Dances That Recommend This Tune"];
          L.div
            (
              let%lwt tune = tune_lwt in
              let%lwt dances = Tune.dances tune in
              Lwt.return
                [
                  if dances = [] then
                    txt "There are no dances that recommend this tune."
                  else
                    Dancelor_client_tables.dances dances
                ]
            )
        ];
    ]
