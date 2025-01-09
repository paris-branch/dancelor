open Dancelor_common_model

type victor_level = One | Two | Three | Four

(** Existing endpoints in Dancelor's API. *)
type endpoint =
  | Set of SetEndpoints.t
  | Version of VersionEndpoints.t
  | Dance of DanceEndpoints.t
  | Victor of victor_level
[@@deriving variants]

(** {2 Routes} *)

open Madge_router
module MQ = Madge_query

let routes : endpoint route list =
  [
    direct `GET "/victor" @@ Victor One;
    direct `GET "/victor2" @@ Victor Two;
    direct `GET "/victor3" @@ Victor Three;
    direct `GET "/victor4" @@ Victor Four
  ] @
  wrap_routes ~prefix: "/set" ~wrap: set ~unwrap: unSet SetEndpoints.routes @
  wrap_routes ~prefix: "/version" ~wrap: version ~unwrap: unVersion VersionEndpoints.routes @
  wrap_routes ~prefix: "/dance" ~wrap: dance ~unwrap: unDance DanceEndpoints.routes

let path ?(api_prefix = true) endpoint =
  let request = Madge_router.resource_to_request endpoint routes in
  assert (request.method_ = `GET);
  let path = Uri.(to_string @@ make ~path: request.path ~query: (MQ.to_strings request.query) ()) in
  (* FIXME: a bit stupid to convert it to string, we should just carry [Uri.t] around! *)
  if api_prefix then "/" ^ Constant.api_prefix ^ path else path

let path_versionLy slug = path @@ version @@ VersionEndpoints.ly slug
let path_versionSvg ?params slug = path @@ version @@ VersionEndpoints.svg slug params
let path_versionOgg slug = path @@ version @@ VersionEndpoints.ogg slug
let path_versionPdf ?params slug = path @@ version @@ VersionEndpoints.pdf slug params
let path_setPdf ?params slug = path @@ set @@ SetEndpoints.pdf slug params
let path_dancePdf ?params slug = path @@ dance @@ DanceEndpoints.pdf slug params

let endpoint method_ path query =
  Madge_router.request_to_resource {method_; path; query} routes

(** NEW-NEW STYLE *)

type (_, _, _) endpoint_new =
  | Person : ('a, 'w, 'r) PersonEndpoints.t -> ('a, 'w, 'r) endpoint_new
  | Book : ('a, 'w, 'r) BookEndpoints.t -> ('a, 'w, 'r) endpoint_new
  | Tune : ('a, 'w, 'r) TuneEndpoints.t -> ('a, 'w, 'r) endpoint_new

type endpoint_new_wrapped =
  | W : ('a, 'r Lwt.t, 'r) endpoint_new -> endpoint_new_wrapped

let all_endpoints =
  List.flatten
    [
      List.map (fun (PersonEndpoints.W e) -> W (Person e)) PersonEndpoints.all;
      List.map (fun (BookEndpoints.W e) -> W (Book e)) BookEndpoints.all;
      List.map (fun (TuneEndpoints.W e) -> W (Tune e)) TuneEndpoints.all;
    ]

open Madge

(* FIXME: Factorise adding the `/api` prefix. *)
let route : type a w r. (a, w, r) endpoint_new -> (a, w, r) route = function
  | Person endpoint -> literal "api" @@ literal "person" @@ PersonEndpoints.route endpoint
  | Book endpoint -> literal "api" @@ literal "book" @@ BookEndpoints.route endpoint
  | Tune endpoint -> literal "api" @@ literal "tune" @@ TuneEndpoints.route endpoint

let path : type a r. (a, string, r) route -> a = fun route ->
  process route (fun (module _) uri -> Uri.to_string uri)
