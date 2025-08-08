open Nes
open Common

open Js_of_ocaml
open Html
open Views

let get_uri () = Uri.of_string (Js.to_string Dom_html.window##.location##.href)

let dispatch uri =
  let dispatch : type a r. (a, Page.t Lwt.t, r) Endpoints.Page.t -> a = function
    | Index -> Index.create ()
    | Explore -> (fun query -> Explorer.create ?query ())
    | Book -> (fun context id -> BookViewer.create ?context id)
    | BookAdd -> BookEditor.create ()
    | BookEdit -> (fun id -> BookEditor.create ~edit: id ())
    | Dance -> (fun context id -> DanceViewer.create ?context id)
    | DanceAdd -> DanceEditor.create ()
    | Person -> (fun context id -> PersonViewer.create ?context id)
    | PersonAdd -> PersonEditor.create ()
    | Version -> (fun context id -> VersionViewer.create ?context id)
    | VersionAdd -> VersionEditor.create ()
    | Tune -> (fun context id -> TuneViewer.create ?context id)
    | TuneAdd -> TuneEditor.create ()
    | Set -> (fun context id -> SetViewer.create ?context id)
    | SetAdd -> SetEditor.create ()
    | Source -> (fun context id -> SourceViewer.create ?context id)
    | SourceAdd -> SourceEditor.create ()
    | UserCreate -> UserCreator.create ()
    | UserPasswordReset -> UserPasswordResetter.create
  in
  let madge_match_apply_all : Page.t Lwt.t Endpoints.Page.wrapped' list -> (unit -> Page.t Lwt.t) option =
    List.map_first_some @@ fun (Endpoints.Page.W' endpoint) ->
    Madge.apply' (Endpoints.Page.route endpoint) (fun () -> dispatch endpoint) {meth = GET; uri; body = ""}
  in
  match madge_match_apply_all @@ Endpoints.Page.all' () with
  | Some page -> page ()
  | None -> OooopsViewer.create `Not_found

let () = Random.self_init ()

let () = Environment.start_ping_routine ()

let () =
  Depart.keep_forever @@
  flip S.map Environment.run_status @@ function
  | Running -> ()
  | Offline ->
    Components.Toast.open_
      ~title: "You are now offline"
      [
        txt "The Dancelor server cannot be reached any more. You are now in offline mode.";
      ]
  | Newer ->
    Components.Toast.open_
      ~title: "Newer version available"
      [txt "The Dancelor server has reloaded, meaning that there might be a newer version of the software and/or the database. You might want to reload the page.";
      ]
      ~buttons: [
        Components.Button.make
          ~label: "Reload"
          ~icon: "arrow-clockwise"
          ~classes: ["btn-primary"]
          ~onclick: (fun () -> Js_of_ocaml.Dom_html.window##.location##reload; lwt_unit)
          ();
      ]

let () =
  let previous_exn = ref (Failure "this is an exception that is never raised") in
  Lwt.async_exception_hook :=
    (fun exn ->
      if exn = !previous_exn then ()
      else
        (
          previous_exn := exn;
          match exn with
          | Lwt.Canceled -> () (* the promises are cancelled on purpose *)
          | MainPage.ReplacementSuccessful -> () (* see comment for {!MainPage.load_sleep_raise} *)
          | Madge_client.(Error (Http {request; status; _})) ->
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
          | Madge_client.(Error (ServerUnreachable {request; status})) ->
            Components.Toast.open_
              ~title: "Server unreachable"
              [
                txt "While querying ";
                a ~a: [a_href (Uri.to_string request.uri)] [txt @@ Uri.path request.uri];
                txt ", the Dancelor server was unreachable (“";
                txt (Cohttp.Code.string_of_status status);
                txt
                  "”), despite several attempts. Is your internet connection \
                     maybe unstable? If the error persists, and your internet \
                     connection works otherwise fine, please contact your \
                     administrator."
              ]
          | exn ->
            Components.Toast.open_
              ~title: "Uncaught exception"
              [
                txt "Dancelor encountered";
                pre ~a: [a_class ["text-wrap"; "my-2"]] [txt @@ Printexc.to_string exn];
                txt
                  "and did not handle it gracefully. If the error persists, please \
               contact your administrator or file a bug report.";
              ]
        )
    )

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ev ->
      MainPage.initialise ();
      Lwt.async (MainPage.load % dispatch % get_uri);
      Js._false
    )
