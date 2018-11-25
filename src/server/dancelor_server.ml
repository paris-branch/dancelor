open Dancelor_common
open Dancelor_controller
open Cohttp_lwt_unix

let check_ezjsonm_t : Ezjsonm.value -> Ezjsonm.t = function
  | `O v -> `O v
  | `A v -> `A v
  | _ -> failwith "QueryHelpers.check_ezjsonm_t"

let respond_html html =
  Server.respond_string ~status:`OK ~body:html ()

let respond_json ?(status=`OK) ?(success=true) json =
  let json =
    match json with
    | `O l -> `O (("success", `Bool success) :: l)
    | _ -> assert false
  in
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

module String = struct
  include String

  let starts_with needle haystack =
    try
      String.sub haystack 0 (String.length needle) = needle
    with
      Invalid_argument _ -> false
end

(* ============================= [ Callbacks ] ============================== *)

let controllers : ('a * string * ('c -> (Ezjsonm.value, string) result Lwt.t)) list =
  [
    ([`GET], "/credit",   Credit.get) ;
    ([`GET], "/person",   Person.get) ;
    ([`GET], "/tune",     Tune.get) ;
    (* ([`GET], "/tune.png", Tune.png) ; *)
    ([`GET], "/set",      Set.get)
  ]

let callback _ request _body =
  let uri = Request.uri request in

  (* We first determine if it is an API call (it starts with "/api")
     and the real path of the request (removing "/api" when it's
     there). *)
  let api, path =
    let path = Uri.path uri in
    Log.(debug_async (spf "Request for path: %s" path));
    if String.starts_with "/api" path then
      (true, String.sub path 4 (String.length path - 4))
    else
      (false, path)
  in
  Log.(debug_async (spf "It is%s an api call of path %s" (if api then "" else " not") path));


  (* We then find the controller corresponding to the given path and
     we execute it. *)
  let controller =
    let method_ = Request.meth request in
    let open OptionMonad in
    List.find_opt
      (fun (methods, path', _) ->
        List.mem method_ methods && path = path')
      controllers
    >>= fun (_, _, controller) -> Some controller
  in

  match controller with
  | Some controller ->
     (
       let%lwt json = controller (Uri.query uri) in

       match json with
       | Ok json ->
          (
            let json = check_ezjsonm_t json in
            Log.(debug_async (spf "Controller output: Ok %s" (Ezjsonm.to_string json)));

            (* If we are in an API call, we just answer the JSON
               provided by the controller. But if not, we try to find
               a view that matches the path and to return it. *)
            if api then
              respond_json json
            else
              (
                try
                  let view = Filename.concat (Unix.getenv "DANCELOR_VIEWS") (path ^ ".html") in
                  let ichan = open_in view in
                  let template = Lexing.from_channel ichan |> Mustache.parse_lx in
                  close_in ichan;
                  respond_html (Mustache.render template json)
                with
                  Sys_error _ ->
                  Log.(debug_async "I did not find any view for this query");
                  Server.respond_not_found ()
              )
          )
       | Error message ->
          (* FIXME: ~status *)
          respond_json ~success:false (`O ["message", `String message])
     )
  | None ->
     (
       Log.(debug_async "I did not find any controller matching this query");
       Server.respond_not_found ()
     )

let callback fixme request body =
  try
    callback fixme request body
  with
    exn ->
     Log.(error_async (spf "%s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())));
     raise exn

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
