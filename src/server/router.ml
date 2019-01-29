open Dancelor_common
open Dancelor_controller
open Cohttp_lwt_unix
module Log = (val Log.create "dancelor.server.router")

module Route = struct
  type component =
    | L of string                 (* Literal(value) *)
    | P of string * string option (* Pattern(name, ext) *)

  type t = component list

  type match_ = (string * string) list

  let match_ (uri : string) (route : t) : match_ option =
    let rec match_ (result : match_) (route : t) = function
      | [] when route = [] -> Some result
      | [] -> None
      | uri_comp :: uri ->
         match route with
         | [] ->
            None
         | route_comp :: route ->
            match route_comp with
            | L route_comp when uri_comp = route_comp ->
               match_ result route uri
            | P (value, None)->
               match_ ((value, uri_comp) :: result) route uri
            | P (value, Some ext) when Filename.check_suffix uri_comp ext ->
               match_ ((value, Filename.chop_suffix uri_comp ext) :: result) route uri
            | _ ->
               None
    in
    uri
    |> String.split_on_char '/'
    |> List.filter ((<>) "")
    |> match_ [] route
end

type query = (string * string list) list
[@@deriving show]

type 'a controller = Route.match_ -> query -> 'a Lwt.t (* FIXME: body, conn? *)

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
    "link" (`String (prefix ^ "/" ^ slug))
    json

let link_adders =
  [ "set", (fun set ->
      let slug = Json.(get ~k:slug ["slug"] set) in
      let link ext = "/set/" ^ slug ^ ext in
      Json.add_fields
        ["link", `String (link "");
         "link_ly", `String (link ".ly");
         "link_pdf", `String (link ".pdf")]
        set) ;

    "credit", out_of_slug "/credit";

    "person", out_of_slug "/person";

    "program", (fun program ->
      let slug = Json.(get ~k:slug ["slug"] program) in
      let link ext = "/program/" ^ slug ^ ext in
      Json.add_fields
        ["link", `String (link "");
         "link_ly", `String (link ".ly");
         "link_pdf", `String (link ".pdf")]
        program) ;

    "tune-group", out_of_slug "/tune-group";

    "tune", (fun tune ->
      let slug = Json.(get ~k:slug ["slug"] tune) in
      let link ext = "/tune/" ^ slug ^ ext in
      tune
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
  fun uri_match query ->
  try%lwt
    let%lwt json = controller uri_match query in
    let json = json_add_links json in
    Log.debug (fun m -> m "JSON controller response: %s" (Json.to_string json));
    respond_json ~status:`OK json
  with
    Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message])

let json_controller_to_html_controller ~view ~controller =
  fun uri_match query ->
  try%lwt
    let%lwt json = controller uri_match query in
    let json = json_add_links json in
    respond_html (View.render view (Json.to_ezjsonm json))
  with
    Error.Error (status, message) -> respond_json ~status ~success:false (`O ["message", `String message]) (* FIXME: error page! *)

let make_raw ?(methods=[`GET]) ~path ~controller () =
  (methods, path, controller)

let make_json ?methods ~path ?controller () =
  let controller =
    match controller with
    | None -> (fun _ _ -> Lwt.return (`O []))
    | Some controller -> controller
  in
  [make_raw ?methods ~path:((Route.L Config.api_prefix) :: path)
     ~controller:(json_controller_to_controller ~controller) ()]

let make_html ?methods ~path ~view ?controller () =
  let controller =
    match controller with
    | None -> (fun _ _ -> Lwt.return (`O []))
    | Some controller -> controller
  in
  [make_raw ?methods ~path
     ~controller:(json_controller_to_html_controller ~view ~controller) ()]

let make_both ?methods ~path ~view ?controller () =
  let controller =
    match controller with
    | None -> (fun _ _ -> Lwt.return (`O []))
    | Some controller -> controller
  in
  [make_raw ?methods ~path:((Route.L Config.api_prefix) :: path)
     ~controller:(json_controller_to_controller ~controller) () ;
   make_raw ?methods ~path
     ~controller:(json_controller_to_html_controller ~view ~controller) ()]

(* ============================ [ Controllers ] ============================= *)

(* The order matters! For instance, /set/all could be considered to be
   a /set/{slug} with [slug = all]. So one has to put the /set/all
   rule before /set/{slug}. The same goes for routes with extensions. *)

let controllers =
  Route.[
      make_html ~path:[] ~view:"/index" () ;

      make_both ~path:[L "credit"; P ("slug", None)] ~view:"/credit" ~controller:Credit.get () ;

      make_html ~path:[L "pascaline"] ~view:"/bad-gateway" () ;

      make_both ~path:[L "person"; P ("slug", None)] ~view:"/person" ~controller:Person.get () ;

      make_both ~path:[L "program"; L "all"] ~view:"/program/all" ~controller:Program.get_all () ;
      [make_raw ~path:[L "program"; P ("slug", Some ".pdf")] ~controller:Program.Pdf.get ()] ;
      make_both ~path:[L "program"; P ("slug", None)] ~view:"/program" ~controller:Program.get () ;

      make_both ~path:[L "set"; L "all"] ~view:"/set/all" ~controller:Set.get_all () ;
      make_html ~path:[L "set"; L "compose"] ~view:"/set/compose" () ;
      make_json ~path:[L "set"; L "save"] ~controller:Set.save () ;
      [make_raw ~path:[L "set"; P ("slug", Some ".ly")] ~controller:Set.Ly.get ()] ;
      [make_raw ~path:[L "set"; P ("slug", Some ".pdf")] ~controller:Set.Pdf.get ()] ;
      make_both ~path:[L "set"; P ("slug", None)] ~view:"/set" ~controller:Set.get () ;

      make_both ~path:[L "tune-group"; P ("slug", None)] ~view:"/tune-group" ~controller:TuneGroup.get () ;

      make_both ~path:[L "tune"; L "all"] ~view:"/tune/all" ~controller:Tune.get_all () ;
      [make_raw ~path:[L "tune"; P ("slug", Some ".ly")] ~controller:Tune.get_ly ()] ;
      [make_raw ~path:[L "tune"; P ("slug", Some ".png")] ~controller:Tune.Png.get ()] ;
      make_both ~path:[L "tune"; P ("slug", None)] ~view:"/tune" ~controller:Tune.get () ;

      [make_raw ~path:[L "victor"] ~controller:(fun _ -> exit 0) ()] ;
  ]
  |> List.flatten
