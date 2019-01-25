open Dancelor_common
open Dancelor_controller
open Cohttp_lwt_unix
module Log = (val Log.create "dancelor.server.router")

type query = (string * string list) list
[@@deriving show]

type 'a controller = query -> 'a Lwt.t (* FIXME: body, conn? *)

type json = [ `O of (string * Json.value) list ]
type generic = Response.t * Cohttp_lwt.Body.t

let respond_html html =
  Server.respond_string ~status:`OK ~body:html ()

let respond_json ?(status=`OK) ?(success=true) (json : json) =
  let `O fields = json in
  let json = `O (("success", `Bool success) :: fields) in
  Server.respond_string ~status ~body:(Json.to_string json) ()

(* =========================== [ json_add_links ] =========================== *)

let out_of_slug prefix json =
  let slug = Slug.from_string Json.(get ~k:string ["slug"] json) in
  Json.add_field
    "link" (`String (prefix ^ "?slug=" ^ slug))
    json

let link_adders =
  [ "set", (fun set ->
      let slug = Json.(get ~k:slug ["slug"] set) in
      let link ext = "/set" ^ ext ^ "?slug=" ^ slug in
      Json.add_fields
        ["link", `String (link "");
         "link_ly", `String (link ".ly");
         "link_pdf", `String (link ".pdf")]
        set) ;

    "credit", out_of_slug "/credit";

    "person", out_of_slug "/person";

    "program", out_of_slug "/program";

    "tune", out_of_slug "/tune";

    "tune-version", (fun version ->
      let tune_slug = Json.(get ~k:slug ["tune-slug"] version) in
      let link ext =
        "/tune/version" ^ ext
        ^ "?slug=" ^ tune_slug
        ^ match Json.(get_opt ~k:slug ["subslug"] version) with
          | Some subslug -> "&subslug=" ^ subslug
          | None -> "&default"
      in
      version
      |> Json.add_fields
           ["link", `String (link "");
            "link_ly", `String (link ".ly");
            "link_png", `String (link ".png")]) ]

let rec json_add_links json =
  match json with
  | `O fields ->
     (
       let json = `O (List.map (fun (field, value) -> (field, json_add_links value)) fields) in
       match Json.(get_opt ~k:string ["type"] json) with
| Some type_ -> Json.to_value (List.assoc type_ link_adders json)
       | None -> json
     )
  | `A jsons ->
     `A (List.map json_add_links jsons)
  | _ ->
     json

let json_add_links json =
  Json.of_value (json_add_links json)

(* ======================== [ JSON controller -> * ] ======================== *)

let json_controller_to_controller ~controller =
  let controller query =
    try%lwt
      let%lwt json = controller query in
      let json = json_add_links json in
      Log.debug (fun m -> m "JSON controller response: %s" (Json.to_string json));
      respond_json ~status:`OK json
    with
      Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message])
  in
  controller

let json_controller_to_html_controller ~view ~controller =
  fun query ->
  try%lwt
    let%lwt json = controller query in
    let json = json_add_links json in
    respond_html (View.render view (Json.to_ezjsonm json))
  with
    Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message]) (* FIXME: error page! *)

let make_raw ?(methods=[`GET]) ~path ~controller () =
  (methods, path, controller)

let make_json ?methods ~path ?controller () =
  let controller =
    match controller with
    | None -> (fun _ -> Lwt.return (`O []))
    | Some controller -> controller
  in
  [make_raw ?methods ~path:(Config.api_prefix ^ path)
     ~controller:(json_controller_to_controller ~controller) ()]

let make_html ?methods ~path ?view ?controller () =
  let view =
    match view with
    | None -> path
    | Some view -> view
  in
  let controller =
    match controller with
    | None -> (fun _ -> Lwt.return (`O []))
    | Some controller -> controller
  in
  [make_raw ?methods ~path
     ~controller:(json_controller_to_html_controller ~view ~controller) ()]

let make_both ?methods ~path ?view ?controller () =
  let view =
    match view with
    | None -> path
    | Some view -> view
  in
  let controller =
    match controller with
    | None -> (fun _ -> Lwt.return (`O []))
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
    make_both ~path:"/credit" ~controller:Credit.get () ;
    make_html ~path:"/pascaline" ~view:"/bad-gateway" () ;
    make_both ~path:"/person" ~controller:Person.get () ;
    make_both ~path:"/program" ~controller:Program.get () ;
    make_both ~path:"/program/all" ~controller:Program.get_all () ;
    make_both ~path:"/set" ~controller:Set.get () ;
    [make_raw ~path:"/set.ly" ~controller:Set.Ly.get ()] ;
    [make_raw ~path:"/set.pdf" ~controller:Set.Pdf.get ()] ;
    make_both ~path:"/set/all" ~controller:Set.get_all () ;
    make_html ~path:"/set/compose" () ;
    make_json ~path:"/set/save" ~controller:Set.save () ;
    make_both ~path:"/tune" ~controller:Tune.get () ;
    make_both ~path:"/tune/all" ~controller:Tune.get_all () ;
    make_both ~path:"/tune/version" ~controller:TuneVersion.get () ;
    [make_raw ~path:"/tune/version.ly" ~controller:TuneVersion.get_ly ()] ;
    [make_raw ~path:"/tune/version.png" ~controller:TuneVersion.Png.get ()] ;
    [make_raw ~path:"/victor" ~controller:(fun _ -> exit 0) ()] ;
  ]
  |> List.flatten
