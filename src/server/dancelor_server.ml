open Dancelor_common
open Cohttp_lwt_unix
module Log = (val Log.create "dancelor.server" : Logs.LOG)

let cleanup_query query =
  let rec remove_empties = function
    | [] -> []
    | (_, [""]) :: t -> remove_empties t
    | x :: t -> x :: remove_empties t
  in
  let rec merge_duplicates = function
    | [] -> []
    | [m] -> [m]
    | (k1, v1) :: (k2, v2) :: query when k1 = k2 ->
       merge_duplicates ((k1, v1 @ v2) :: query)
    | (k, v) :: query ->
       (k, v) :: merge_duplicates query
  in
  query
  |> remove_empties
  (* FIXME: Before sorting, shouldn't we rename key[] into key? *)
  |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)
  |> merge_duplicates

let log_exn ~msg exn =
  Log.err (fun m -> m "%s@\n%s@\n%a" msg (Printexc.to_string exn) pp_string_multiline (Printexc.get_backtrace ()))

let callback _ request _body =
  (* We have a double try ... with to catch all non-Lwt and Lwt
     exceptions. *)
  try
    try%lwt
      let uri = Request.uri request in
      let meth = Request.meth request in
      let path = Uri.path uri in
      Log.info (fun m -> m "Request for %s" path);
      let rec find_controller = function
        | [] ->
           Log.debug (fun m -> m "No controller found for path %s" path);
           Server.respond_not_found ()
        | (methods, route, controller) :: controllers when List.mem meth methods ->
           (
             match Router.Route.match_ path route with
             | None ->
                find_controller controllers
             | Some uri_match ->
                let query = Uri.query uri in
                Log.debug (fun m -> m "Query before cleanup: %s" (Router.show_query query));
                let query = cleanup_query query in
                Log.debug (fun m -> m "Query: %s" (Router.show_query query));
                controller uri_match query
           )
        | _ :: controllers ->
           find_controller controllers
      in
      find_controller Router.controllers
    with
      exn ->
      log_exn ~msg:"Uncaught exception in the callback" exn;
      Server.respond_error ~status:`Internal_server_error ~body:"internal server error" ()
  with
    exn ->
    log_exn ~msg:"Uncaught Lwt exception in the callback" exn;
    Server.respond_error ~status:`Internal_server_error ~body:"internal server error" ()

let () =
  Lwt.async_exception_hook :=
    (function
     | Unix.Unix_error(Unix.EPIPE, _, _) ->
        Log.warn (fun m -> m "Connection closed by the client")
     | exn ->
        log_exn ~msg:"Uncaught asynchronous exception" exn)

let () =
  Log.info (fun m -> m "Initialising database");
  Dancelor_model.Database.initialise ();

  Log.info (fun m -> m "Starting routines");
  Routine.initialise ();

  Log.info (fun m -> m "Starting server");
  let server =
    Lwt.catch
      (fun () ->
        Server.create
          ~mode:(`TCP (`Port Config.port))
          (Server.make ~callback ()))
      (fun exn ->
        log_exn ~msg:"Uncaught Lwt exception in the server" exn;
        Lwt.return ())
  in
  Log.info (fun m -> m "Server is up and running");
  try
    Lwt_main.run server
  with
    exn ->
    log_exn ~msg:"Uncaught exception in the server" exn
