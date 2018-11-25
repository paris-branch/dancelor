open Dancelor_common
open Dancelor_controller
open Cohttp_lwt_unix

type query = (string * string list) list
[@@deriving show]

type 'a controller = query -> Cohttp_lwt.Body.t -> 'a Lwt.t (* FIXME: Server.conn? *)

type json = [ `O of (string * Ezjsonm.value) list ]
type generic = Response.t * Cohttp_lwt.Body.t

let json_to_ezjsonm (json : json) : Ezjsonm.t =
  let `O fields = json in `O fields

let respond_html html =
  Server.respond_string ~status:`OK ~body:html ()

let respond_json ?(status=`OK) ?(success=true) (json : json) =
  let `O fields = json in
  let json = `O (("success", `Bool success) :: fields) in
  Server.respond_string ~status ~body:(Ezjsonm.to_string json) ()

(* ======================== [ JSON controller -> * ] ======================== *)

let json_controller_to_controller ((methods, path, controller) : Cohttp.Code.meth list * string * json controller) : Cohttp.Code.meth list * string * generic controller =
  let controller query body =
    try%lwt
      let%lwt json = controller query body in
      Log.(debug_async (spf "JSON controller response: %s" (Ezjsonm.to_string (json_to_ezjsonm json))));
      respond_json ~status:`OK json
    with
      Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message])
  in
  (methods, "/api" ^ path, controller)

let json_controller_to_html_controller ((methods, path, controller) : Cohttp.Code.meth list * string * json controller) : (Cohttp.Code.meth list * string * generic controller) option =
  try
    let view = Filename.concat (Unix.getenv "DANCELOR_VIEWS") (path ^ ".html") in
    let ichan = open_in view in
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    close_in ichan;

    let controller query body =
      try%lwt
        let%lwt json = controller query body in
        respond_html (Mustache.render template (json_to_ezjsonm json))
      with
        Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message]) (* FIXME: error page! *)
    in
    Some (methods, path, controller)

  with
    Sys_error _ ->
    Log.(debug_async "I did not find any view for this query");
    None

(* ============================ [ Controllers ] ============================= *)

let json_controllers : (Cohttp.Code.meth list * string * json controller) list =
  [
    ([`GET], "/credit",   Credit.get) ;
    ([`GET], "/person",   Person.get) ;
    ([`GET], "/tune",     Tune.get) ;
    ([`GET], "/set",      Set.get) ;
    ([`GET], "/set/compose", Set.compose) ; (* FIXME: this API point is pointless *)
  ]

let controllers : (Cohttp.Code.meth list * string * generic controller) list =
  List.map json_controller_to_controller json_controllers
  @ List.map_filter json_controller_to_html_controller json_controllers
  @ [
      ([`GET], "/tune.png", Tune.png) ;
    ]

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
       Log.(debug_async (spf "Query: %s" (show_query query)));
       controller query body;
    | None ->
       Log.(debug_async (spf "No controller found for path %s" path));
       Server.respond_not_found ()
  with
    exn ->
     Log.(error_async (spf "Uncaught exception: %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())));
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
