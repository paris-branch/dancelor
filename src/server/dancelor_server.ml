open Cohttp_lwt_unix

let respond ?(status=`OK) json =
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

let success = function
  | `O l -> `O (("success", `Bool true) :: l) |> respond
  | _ -> assert false

let error msg =
  (* FIXME: error code *)
  `O [
      "success", `Bool false;
      "message", `String msg
    ]
  |> respond

(* =========================== [ Main Callback ] ============================ *)

let callback _ request _body =
  let path = Uri.path (Request.uri request) in
  let meth = Request.meth request in

  match meth, path with
  | `GET, "/" ->
     let uri = Request.uri request in
     `O [
         "uri", `String (Uri.to_string uri);
         "path", `String (Uri.path uri);
         "path_and_query", `String (Uri.path_and_query uri);
         "query",
         `O (
             Uri.query uri
             |> List.map
                  (fun (s, sl) ->
                    (s, `A (List.map (fun s -> `String s) sl)))
           )
       ]
     |> success

  | _ ->
     error ("not found: " ^ path)



(* ============================== [ Options ] =============================== *)

let port = 8080

let () =
  let server =
    Server.create
      ~mode:(`TCP (`Port port))
      (Server.make ~callback ())
  in
  ignore (Lwt_main.run server)
