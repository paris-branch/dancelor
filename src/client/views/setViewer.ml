open Nes
open Common

open Model
open Html

let create ?context slug =
  MainPage.get_model_or_404 (Set Get) slug @@ fun set ->
  Lwt.return @@
    Page.make
      ~parent_title: "Set"
      ~title: (S.const @@ Set.name' set)
      ~before_title: [
        Components.ContextLinks.make_and_render
          ?context
          ~this_page: (Endpoints.Page.href_set slug)
          (Lwt.return @@ Any.set set);
      ]
      [
        h5 ~a: [a_class ["text-center"]] [Formatters.Set.works' set];
        h5
          ~a: [a_class ["text-center"]]
          [
            txt ((Kind.Dance.to_pretty_string % Set.kind') set);
            txt " — Play ";
            txt ((SetOrder.to_pretty_string % Set.order') set);
          ];
        h5
          ~a: [a_class ["text-center"]]
          [
            with_span_placeholder @@
              match%lwt Set.conceptors' set with
              | [] -> Lwt.return_nil
              | devisers -> Lwt.return [txt "Set by "; Formatters.Person.names' ~links: true devisers]
          ];
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
                        a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (SetDownloadDialog.create_and_open slug)); false);
                      ]
                      [
                        i ~a: [a_class ["bi"; "bi-file-pdf"]] [];
                        txt " Download PDF";
                      ];
                  ];
                li
                  [
                    a
                      ~a: [
                        a_class ["dropdown-item"];
                        a_href Endpoints.Page.(href BookAdd);
                        a_onclick (fun _ -> BookEditor.Editor.add_to_storage slug; true);
                      ]
                      [
                        i ~a: [a_class ["bi"; "bi-plus-square"]] [];
                        txt " Add to current book";
                      ];
                  ];
              ];
          ];
        p
          [
            txt
              (
                match Set.instructions' set with
                | "" -> ""
                | instructions -> "Instructions: " ^ instructions
              )
          ];
        R.div
          (
            S.from'
              [
                div
                  ~a: [a_class ["text-center"; "mt-4"]]
                  [
                    h4 [span_placeholder ()];
                    div_placeholder ~min: 10 ~max: 20 ();
                  ];
                div
                  ~a: [a_class ["text-center"; "mt-4"]]
                  [
                    h4 [span_placeholder ()];
                    div_placeholder ~min: 10 ~max: 20 ();
                  ];
              ] @@
              let%lwt contents = Set.contents' set in
              Lwt_list.mapi_p
                (fun index (version, _parameters) ->
                  let context = Endpoints.Page.inSet slug index in
                  (* FIXME: use parameters *)
                  let%lwt tune = Version.tune' version in
                  let slug = Entry.slug version in
                  Lwt.return @@
                    div
                      ~a: [a_class ["text-center"; "mt-4"]]
                      [
                        h4 [a ~a: [a_href (Endpoints.Page.href_version ~context slug)] [txt @@ Tune.name' tune]];
                        Components.VersionSvg.make slug;
                      ]
                )
                contents
          );
        Utils.quick_explorer_links'
          (Lwt.return set)
          [
            ("books containing this set", Filter.(Any.book' % Book.memSet'));
          ];
      ]
