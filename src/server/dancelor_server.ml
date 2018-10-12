open Dancelor_common
open Cohttp_lwt_unix
open Common

(* ============================= [ Callbacks ] ============================== *)

let callbacks =
  [
    (* API points *)
    ([`GET], "/api/credit", (Credit.Api.get ||> respond_json)) ;
    ([`GET], "/api/person", (Person.Api.get ||> respond_json)) ;
    ([`GET], "/api/tune", (Tune.Api.get ||> respond_json)) ;

    (* HTML *)
    ([`GET], "/credit", Credit.Html.get) ;
    ([`GET], "/person", Person.Html.get) ;
    ([`GET], "/tune", Tune.Html.get) ;
  ]

let callback _ request _body =
  let uri = Request.uri request in
  let path = Uri.path (Request.uri request) in
  let meth = Request.meth request in

  let rec find_callback = function
    | [] ->
       error ("not found: " ^ path)
       |> respond_json

    | (methods, path', callback) :: _
         when List.mem meth methods && path = path' ->
       callback (Uri.query uri)

    | _ :: callbacks -> find_callback callbacks
  in

  try
    find_callback callbacks
  with
  | Common.ArgumentRequired arg ->
     error ("the argument '" ^arg^ "' is required")
     |> respond_json
  | Common.ArgumentOfWrongType (typ, arg) ->
     error ("the argument '" ^arg^ "' is expected to be of type " ^typ)
     |> respond_json
  | Common.TooManyArguments arg ->
     error ("only one argument '" ^arg^ "' is expected")
     |> respond_json

  | exn ->
    Format.eprintf "Unhandled exception: %s@." (Printexc.to_string exn);
    error ("internal server error")
    |> respond_json

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
