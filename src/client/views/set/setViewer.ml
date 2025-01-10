open Nes
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Components = Dancelor_client_components
module Page = Dancelor_client_page
module Utils = Dancelor_client_utils
open Dancelor_client_html

let create ?context slug =
  let set_lwt = Set.get slug in
  let title = S.from' "" (Lwt.map Set.name set_lwt) in
  Page.make ~title: (Page.sub_title "Set" title) @@
  div
    [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (PageRouter.path_set slug)
        (Lwt.map Any.set set_lwt);
      h2 ~a: [a_class ["title"]] [R.txt title];
      L.h3 ~a: [a_class ["title"]] (set_lwt >>=| Formatters.Set.works);
      h3
        ~a: [a_class ["title"]]
        [
          L.txt (Lwt.map (Kind.Dance.to_pretty_string % Set.kind) set_lwt);
          txt " — Play ";
          L.txt (Lwt.map (SetOrder.to_pretty_string % Set.order) set_lwt);
        ];
      L.h3
        ~a: [a_class ["title"]]
        (
          match%lwt set_lwt >>=| Set.conceptors with
          | [] -> Lwt.return_nil
          | devisers -> Lwt.return (txt "Set by " :: Formatters.Person.names ~link: true devisers)
        );
      div
        ~a: [a_class ["buttons"]]
        [
          a
            ~a: [
              a_class ["button"];
              a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (SetDownloadDialog.create_and_open slug)); false);
            ]
            [
              i ~a: [a_class ["material-symbols-outlined"]] [txt "picture_as_pdf"];
              txt " PDF";
            ];
          a
            ~a: [
              a_class ["button"];
              a_href PageRouter.(path_new @@ route BookAdd);
              a_onclick (fun _ -> BookEditor.Editor.add_to_storage slug; true);
            ]
            [
              i ~a: [a_class ["material-symbols-outlined"]] [txt "add_box"];
              txt " Add to current book";
            ];
        ];
      p
        [
          L.txt
            (
              match%lwt Lwt.map Set.instructions set_lwt with
              | "" -> Lwt.return ""
              | instructions -> Lwt.return ("Instructions: " ^ instructions)
            )
        ];
      div ~a: [a_class ["after-buttons"]] [];
      div
        ~a: [a_class ["section"]]
        [
          L.div
            (
              let%lwt set = set_lwt in
              let%lwt contents = Set.contents set in
              Lwt_list.mapi_p
                (fun index (version, _parameters) ->
                   let context = PageRouter.inSet slug index in
                   (* FIXME: use parameters *)
                   let%lwt tune = Version.tune version in
                   let slug = Version.slug version in
                   Lwt.return
                     (
                       div
                         ~a: [a_class ["image-container"]]
                         [
                           h4 [a ~a: [a_href PageRouter.(path_version ~context slug)] [txt @@ Tune.name tune]];
                           object_
                             ~a: [
                               a_mime_type "image/svg+xml";
                               a_data (ApiRouter.(path @@ route @@ Version Svg) None slug);
                             ]
                             [];
                         ]
                     )
                )
                contents
            );
        ];
      Utils.quick_explorer_links'
        set_lwt
        [
          ("books containing this set", Any.Filter.book' % Book.Filter.memSet');
        ];
    ]
