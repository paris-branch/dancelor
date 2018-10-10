open Cohttp_lwt_unix

let respond ?(status=`OK) json =
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

let success = function
  | `O l -> `O (("success", `Bool true) :: l) |> respond
  | _ -> assert false

let error status msg =
  `O [
      "success", `Bool false;
      "message", `String msg
    ]
  |> respond ~status

(* ============================= [ Callbacks ] ============================== *)

let callbacks =
  [ ([`GET], "/credit", Credit.get) ;
    ([`GET], "/tune/png", Tune.get_png) ]

let callback _ request _body =
  let uri = Request.uri request in
  let path = Uri.path (Request.uri request) in
  let meth = Request.meth request in

  let rec find_callback = function
    | [] ->
       error `Not_found ("not found: " ^ path)

    | (methods, path', callback) :: _
         when List.mem meth methods && path = path' ->
       callback (Uri.query uri)

    | _ :: callbacks -> find_callback callbacks
  in

  try
    find_callback callbacks
  with
  | Common.ArgumentRequired arg ->
     error `Bad_request ("the argument '" ^arg^ "' is required")
  | Common.ArgumentOfWrongType (typ, arg) ->
     error `Bad_request ("the argument '" ^arg^ "' is expected to be of type " ^typ)
  | Common.TooManyArguments arg ->
     error `Bad_request ("only one argument '" ^arg^ "' is expected")

  | exn ->
    Format.eprintf "Unhandled exception: %s@." (Printexc.to_string exn);
    error `Internal_server_error ("internal server error")

(* ============================== [ Options ] =============================== *)

let port = 8080

let () =
  Dancelor_model.Database.initialise ();
  let server =
    Server.create
      ~mode:(`TCP (`Port port))
      (Server.make ~callback ())
  in
  ignore (Lwt_main.run server)
