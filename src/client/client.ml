open Nes
open Common

open Js_of_ocaml
open Html
open Views

let get_uri () = Uri.of_string (Js.to_string Dom_html.window##.location##.href)

let dispatch uri =
  let dispatch : type a r. (a, Page.t, r) Endpoints.Page.t -> a = function
    | Index -> Index.create ()
    | Explore -> (fun query -> Explorer.create ?query ())
    | Book -> (fun context slug -> BookViewer.create ?context slug)
    | BookAdd -> BookEditor.create ()
    | BookEdit -> (fun slug -> BookEditor.create ~edit: slug ())
    | Dance -> (fun context slug -> DanceViewer.create ?context slug)
    | DanceAdd -> DanceEditor.create ()
    | Person -> (fun context slug -> PersonViewer.create ?context slug)
    | PersonAdd -> PersonEditor.create ()
    | Version -> (fun context slug -> VersionViewer.create ?context slug)
    | VersionAdd -> (fun tune -> VersionEditor.create ~tune: (Option.to_list tune) ())
    | Tune -> (fun context slug -> TuneViewer.create ?context slug)
    | TuneAdd -> TuneEditor.create ()
    | Set -> (fun context slug -> SetViewer.create ?context slug)
    | SetAdd -> SetEditor.create ()
    | Source -> (fun context slug -> SourceViewer.create ?context slug)
    | SourceAdd -> SourceEditor.create ()
  in
  let madge_match_apply_all : Page.t Endpoints.Page.wrapped' list -> (unit -> Page.t) option =
    List.map_first_some @@ fun (Endpoints.Page.W endpoint) ->
    Madge.match_' (Endpoints.Page.route endpoint) (dispatch endpoint) {meth = GET; uri; body = ""}
  in
  match madge_match_apply_all Endpoints.Page.all_endpoints' with
  | Some page -> page ()
  | None -> OooopsViewer.create `Not_found

let () =
  Lwt.async_exception_hook :=
    (function
      | Lwt.Canceled -> () (* the promises are cancelled on purpose *)
      | Madge_cohttp_lwt_client.HttpError {request; status; _} ->
        Components.Toast.open_
          ~title: "Uncaught API call error"
          [
            txt "While querying ";
            a ~a: [a_href (Uri.to_string request.uri)] [txt @@ Uri.path request.uri];
            txt ", Dancelor encountered “";
            txt (Cohttp.Code.string_of_status status);
            txt
              "” and did not handle it gracefully. If the error persists, please \
               contact your administrator or file a bug report.";
          ]
      | exn ->
        Components.Toast.open_
          ~title: "Uncaught exception"
          [
            txt "Dancelor encountered";
            pre [txt @@ Printexc.to_string exn];
            txt
              "and did not handle it gracefully. If the error persists, please \
               contact your administrator or file a bug report.";
          ]
    )

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ev ->
      MainPage.initialise ();
      MainPage.load @@ dispatch @@ get_uri ();
      Js._false
    )
