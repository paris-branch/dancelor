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

let json_controller_to_html_controller ((methods, path, controller) : Cohttp.Code.meth list * string * json controller) : Cohttp.Code.meth list * string * generic controller =
  try
    let view = Filename.concat Config.views (path ^ ".html") in
    let ichan = open_in view in
    Format.eprintf "about to parse %s@." view;
    let template = Lexing.from_channel ichan |> Mustache.parse_lx in
    Format.eprintf "parsed@.";
    close_in ichan;

    let controller query body =
      try%lwt
        let%lwt json = controller query body in
        respond_html (Mustache.render template (json_to_ezjsonm json))
      with
        Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message]) (* FIXME: error page! *)
    in
    (methods, path, controller)

  with
    Sys_error _ ->
    Log.(error_async (Format.sprintf "No view found for %s" path));
    failwith "json_controller_to_html_controller"

(* ============================ [ Controllers ] ============================= *)

let json_controllers : (Cohttp.Code.meth list * string * json controller) list =
  [
  ]

let html_controllers : (Cohttp.Code.meth list * string * json controller) list =
  [
    ([`GET], "/set/compose", Set.compose) ;
  ]

let both_controllers : (Cohttp.Code.meth list * string * json controller) list =
  [
    ([`GET], "/credit",   Credit.get) ;
    ([`GET], "/person",   Person.get) ;
    ([`GET], "/tune",     Tune.get) ;
    ([`GET], "/tune/all", Tune.get_all) ;
    ([`GET], "/set",      Set.get) ;
    ([`GET], "/set/all",  Set.get_all) ;
  ]

let raw_controllers : (Cohttp.Code.meth list * string * generic controller) list =
  [
    ([`GET], "/tune.png", Tune.png) ;
  ]
