open Dancelor_common
open Dancelor_controller
open Cohttp_lwt_unix
module Log = (val Log.create "dancelor.server.router")

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

let json_controller_to_controller ~controller =
  let controller query body =
    try%lwt
      let%lwt json = controller query body in
      Log.debug (fun m -> m "JSON controller response: %s" (Ezjsonm.to_string (json_to_ezjsonm json)));
      respond_json ~status:`OK json
    with
      Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message])
  in
  controller

let json_controller_to_html_controller ~view ~controller =
  fun query body ->
  try%lwt
    let%lwt json = controller query body in
    respond_html (View.render view (json_to_ezjsonm json))
  with
    Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message]) (* FIXME: error page! *)

let make_raw ?(methods=[`GET]) ~path ~controller () =
  (methods, path, controller)

let make_html ?methods ~path ?view ?controller () =
  let view =
    match view with
    | None -> path
    | Some view -> view
  in
  let controller =
    match controller with
    | None -> (fun _ _ -> Lwt.return (`O []))
    | Some controller -> controller
  in
  [make_raw ?methods ~path
     ~controller:(json_controller_to_html_controller ~view ~controller) ()]

let make_both ?methods ~path ?view ?(controller : json controller option) () =
  let view =
    match view with
    | None -> path
    | Some view -> view
  in
  let controller =
    match controller with
    | None -> (fun _ _ -> Lwt.return (`O []))
    | Some controller -> controller
  in
  [make_raw ?methods ~path:(Config.api_prefix ^ path)
     ~controller:(json_controller_to_controller ~controller) () ;
   make_raw ?methods ~path
     ~controller:(json_controller_to_html_controller ~view ~controller) ()]

(* ============================ [ Controllers ] ============================= *)

let controllers =
  [
    make_html ~path:"/" ~view:"/index" () ;
    make_html ~path:"/set/compose" ~controller:Set.compose () ;
    make_both ~path:"/credit" ~controller:Credit.get () ;
    make_both ~path:"/person" ~controller:Person.get () ;
    make_both ~path:"/tune" ~controller:Tune.get () ;
    make_both ~path:"/tune/all" ~controller:Tune.get_all () ;
    make_both ~path:"/set" ~controller:Set.get () ;
    make_both ~path:"/set/all" ~controller:Set.get_all () ;
    [make_raw ~path:"/tune.png" ~controller:Tune.Png.get ()] ;
    [make_raw ~path:"/tune.ly" ~controller:Tune.get_ly ()] ;
  ]
  |> List.flatten
