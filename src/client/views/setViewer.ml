open Nes
open Common

open Model
open Html

let create ?context slug =
  let set_lwt = Set.get slug in
  let title = S.from' "" (Lwt.map Set.name set_lwt) in
  Page.make
    ~parent_title: "Set"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_set slug)
        (Lwt.map Any.set set_lwt);
    ]
    [
      L.h5 ~a: [a_class ["text-center"]] (set_lwt >>=| Formatters.Set.works);
      h5
        ~a: [a_class ["text-center"]]
        [
          L.txt (Lwt.map (Kind.Dance.to_pretty_string % Set.kind) set_lwt);
          txt " — Play ";
          L.txt (Lwt.map (SetOrder.to_pretty_string % Set.order) set_lwt);
        ];
      L.h5
        ~a: [a_class ["text-center"]]
        (
          match%lwt set_lwt >>=| Set.conceptors with
          | [] -> Lwt.return_nil
          | devisers -> Lwt.return (txt "Set by " :: Formatters.Person.names ~link: true devisers)
        );
      div
        ~a: [a_class ["btn-group"; "d-flex"; "justify-content-end"]]
        [
          div
            [
              a
                ~a: [
                  a_class ["btn"; "btn-secondary"];
                  a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (SetDownloadDialog.create_and_open slug)); false);
                ]
                [
                  i ~a: [a_class ["bi"; "bi-file-pdf"]] [];
                  txt " PDF";
                ];
              a
                ~a: [
                  a_class ["btn"; "btn-secondary"];
                  a_href Endpoints.Page.(href BookAdd);
                  a_onclick (fun _ -> BookEditor.Editor.add_to_storage slug; true);
                ]
                [
                  i ~a: [a_class ["bi"; "bi-plus-square"]] [];
                  txt " Add to current book";
                ];
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
                   let context = Endpoints.Page.inSet slug index in
                   (* FIXME: use parameters *)
                   let%lwt tune = Version.tune version in
                   let slug = Entry.slug version in
                   Lwt.return @@
                   div
                     ~a: [a_class ["text-center"]]
                     [
                       h4 [a ~a: [a_href (Endpoints.Page.href_version ~context slug)] [txt @@ Tune.name tune]];
                       Components.VersionSvg.make slug;
                     ]
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
