open Dancelor_common
open Cohttp_lwt_unix
module Log = (val Log.create "dancelor.server" : Logs.LOG)

let rec cleanup_query = function
  | [] -> []
  | (_, [""]) :: t -> cleanup_query t
  | x :: t -> x :: cleanup_query t

let callback _ request body =
  let uri = Request.uri request in
  let meth = Request.meth request in
  let path = Uri.path uri in
  try
    let controller =
      List.find_opt (fun (methods, path', _) ->
          List.mem meth methods && path' = path)
        Router.controllers
    in
    match controller with
    | Some (_, _, controller) ->
       let query = Uri.query uri |> cleanup_query in
       Log.debug (fun m -> m "Query: %s" (Router.show_query query));
       controller query body;
    | None ->
       Log.debug (fun m -> m "No controller found for path %s" path);
       Server.respond_not_found ()
  with
    exn ->
    Log.err (fun m -> m "Uncaught exception: %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ()));
    raise exn

let () =
  Dancelor_model.Database.initialise ();
  let server =
    Server.create
      ~mode:(`TCP (`Port Config.port))
      (Server.make ~callback ())
  in
  ignore (Lwt_main.run server)
