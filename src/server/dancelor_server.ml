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

let callback _ request _body =
  (* We have a double try ... with to catch all non-Lwt and Lwt
     exceptions. *)
  try
    try%lwt
      let uri = Request.uri request in
      let meth = Request.meth request in
      let path = Uri.path uri in
      Log.info (fun m -> m "Request for %s" path);
      let controller =
        List.find_opt (fun (methods, path', _) ->
            List.mem meth methods && path' = path)
          Router.controllers
      in
      match controller with
      | Some (_, _, controller) ->
         let query = Uri.query uri in
         Log.debug (fun m -> m "Query before cleanup: %s" (Router.show_query query));
         let query = cleanup_query query in
         Log.debug (fun m -> m "Query: %s" (Router.show_query query));
         controller query;
      | None ->
         Log.debug (fun m -> m "No controller found for path %s" path);
         Server.respond_not_found ()
    with
      exn ->
      Log.err (fun m -> m "Uncaught exception: %s@\n%a" (Printexc.to_string exn) pp_string_multiline (Printexc.get_backtrace ()));
      Server.respond_error ~status:`Internal_server_error ~body:"internal server error" ()
  with
    exn ->
    Log.err (fun m -> m "Uncaught exception: %s@\n%a" (Printexc.to_string exn) pp_string_multiline (Printexc.get_backtrace ()));
    Server.respond_error ~status:`Internal_server_error ~body:"internal server error" ()

let () =
  Dancelor_model.Database.initialise ();
  let server =
    Server.create
      ~mode:(`TCP (`Port Config.port))
      (Server.make ~callback ())
  in
  Log.info (fun m -> m "Up and running");
  ignore (Lwt_main.run server)
