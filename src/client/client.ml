open Nes
open Common
open Js_of_ocaml
open Html
open Views

let get_uri () = Uri.of_string (Js.to_string Dom_html.window##.location##.href)

let redirect_any id =
  MainPage.madge_call_or_404 (Any Get) id @@ fun any ->
  RedirectionViewer.create (Endpoints.Page.href_any_full any)

let () = Madge_client.initialise_batch_route Endpoints.Api.(route_full Batch)

let dispatch uri =
  let dispatch : type a r. (a, Page.t Lwt.t, r) Endpoints.Page.t -> a = function
    | Index -> Index.create ()
    | Any -> redirect_any
    | Explore -> (fun query -> Explorer.create ?query ())
    | Book -> (fun context id -> BookViewer.create ?context id)
    | Book_add -> BookEditor.create Create_with_local_storage
    | Book_edit -> BookEditor.create <=< (Components.Editor.edit % Option.get <%> Model.Book.get)
    | Dance -> (fun context id -> DanceViewer.create ?context id)
    | Dance_add -> DanceEditor.create Create_with_local_storage
    | Dance_edit -> DanceEditor.create <=< (Components.Editor.edit % Option.get <%> Model.Dance.get)
    | Person -> (fun context id -> PersonViewer.create ?context id)
    | Person_add -> PersonEditor.create Create_with_local_storage
    | Person_edit -> PersonEditor.create <=< (Components.Editor.edit % Option.get <%> Model.Person.get)
    | Version -> (fun context id -> VersionViewer.create ?context id)
    | Version_add -> VersionEditor.create Create_with_local_storage
    | Version_edit -> VersionEditor.create <=< (Components.Editor.edit % Option.get <%> Model.Version.get)
    | Tune -> (fun context id -> TuneViewer.create ?context id)
    | Tune_add -> TuneEditor.create Create_with_local_storage
    | Tune_edit -> TuneEditor.create <=< (Components.Editor.edit % Option.get <%> Model.Tune.get)
    | Set -> (fun context id -> SetViewer.create ?context id)
    | Set_add -> SetEditor.create Create_with_local_storage
    | Set_edit -> SetEditor.create <=< (Components.Editor.edit % Option.get <%> Model.Set.get)
    | Source -> (fun context id -> SourceViewer.create ?context id)
    | Source_add -> SourceEditor.create Create_with_local_storage
    | Source_edit -> SourceEditor.create <=< (Components.Editor.edit % Option.get <%> Model.Source.get)
    | User_create -> UserCreator.create ()
    | User_password_reset -> UserPasswordResetter.create
  in
  let madge_match_apply_all : Page.t Lwt.t Endpoints.Page.wrapped' list -> (unit -> Page.t Lwt.t) option =
    List.map_first_some @@ fun (Endpoints.Page.W' endpoint) ->
    Madge.apply' (Endpoints.Page.route endpoint) (fun () -> dispatch endpoint) (Madge.Request.make ~meth: GET ~uri ~body: "")
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
    Utils.Toast.open_
      ~title: "You are now offline"
      [
        txt
          "The Dancelor server cannot be reached any more. You are now in \
           offline mode."
      ]
  | Newer ->
    Utils.Toast.open_
      ~type_: Forever
      ~title: "Newer version available"
      [txt
        "The Dancelor server has reloaded, meaning that there might be a newer \
         version of the software and/or the database. You might want to reload \
         the page.";
      ]
      ~buttons: [
        Utils.Button.make
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
            Utils.Toast.open_
              ~type_: Forever
              ~title: "Uncaught API call error"
              [
                txt "While querying ";
                a ~a: [a_href (Uri.to_string @@ Madge.Request.uri request)] [txt @@ Uri.path @@ Madge.Request.uri request];
                txt ", Dancelor encountered “";
                txt (Cohttp.Code.string_of_status status);
                txt
                  "” and did not handle it gracefully. If the error persists, please \
               contact your administrator or file a bug report.";
              ]
          | Madge_client.(Error (Server_unreachable {request; status})) ->
            Utils.Toast.open_
              ~type_: Forever
              ~title: "Server unreachable"
              [
                txt "While querying ";
                a ~a: [a_href (Uri.to_string @@ Madge.Request.uri request)] [txt @@ Uri.path @@ Madge.Request.uri request];
                txt ", the Dancelor server was unreachable (“";
                txt (Cohttp.Code.string_of_status status);
                txt
                  "”), despite several attempts. Is your internet connection \
                     maybe unstable? If the error persists, and your internet \
                     connection works otherwise fine, please contact your \
                     administrator."
              ]
          | exn ->
            Utils.Toast.open_
              ~type_: Forever
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
