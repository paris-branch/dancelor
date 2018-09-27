open Cohttp_lwt_unix

let respond_json ?(status=`OK) json =
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

(* =========================== [ Main Callback ] ============================ *)

let callback _ request _body =
  respond_json (`String (request |> Request.uri |> Uri.to_string))

(* ============================== [ Options ] =============================== *)

let port = 8080

let () =
  let server =
    Server.create
      ~mode:(`TCP (`Port port))
      (Server.make ~callback ())
  in
  ignore (Lwt_main.run server)
