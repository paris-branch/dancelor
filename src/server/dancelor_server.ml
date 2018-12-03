open Dancelor_common
open Cohttp_lwt_unix

let controllers : (Cohttp.Code.meth list * string * Router.generic Router.controller) list =
  let open Router in
  List.map json_controller_to_controller (json_controllers @ both_controllers)
  @ List.map json_controller_to_html_controller (html_controllers @ both_controllers)
  @ raw_controllers

let callback _ request body =
  let uri = Request.uri request in
  let meth = Request.meth request in
  let path = Uri.path uri in
  try
    let controller =
      List.find_opt (fun (methods, path', _) ->
          List.mem meth methods && path' = path)
        controllers
    in
    match controller with
    | Some (_, _, controller) ->
       let query = Uri.query uri in
       Log.(debug_async (spf "Query: %s" (Router.show_query query)));
       controller query body;
    | None ->
       Log.(debug_async (spf "No controller found for path %s" path));
       Server.respond_not_found ()
  with
    exn ->
     Log.(error_async (spf "Uncaught exception: %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())));
     raise exn

let port = 8080

let () =
  Dancelor_model.Database.initialise ();
  let server =
    Server.create
      ~mode:(`TCP (`Port port))
      (Server.make ~callback ())
  in
  ignore (Lwt_main.run server)
