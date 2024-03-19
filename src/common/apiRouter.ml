open Dancelor_common_model

type victor_level = One | Two | Three | Four

(** Existing endpoints in Dancelor's API. *)
type endpoint =
  | Book    of    BookEndpoints.t
  | Set     of     SetEndpoints.t
  | Version of VersionEndpoints.t
  | Victor of victor_level
[@@deriving variants]

(** {2 Routes} *)

open Madge_router
module MQ = Madge_query

let routes : endpoint route list =
  [ direct `GET "/victor"  @@ Victor One ;
    direct `GET "/victor2" @@ Victor Two ;
    direct `GET "/victor3" @@ Victor Three ;
    direct `GET "/victor4" @@ Victor Four ]
  @ wrap_routes ~prefix:"/book"    ~wrap:book    ~unwrap:unBook       BookEndpoints.routes
  @ wrap_routes ~prefix:"/set"     ~wrap:set     ~unwrap:unSet         SetEndpoints.routes
  @ wrap_routes ~prefix:"/version" ~wrap:version ~unwrap:unVersion VersionEndpoints.routes

let path ?(api_prefix=true) endpoint =
  let request = Madge_router.resource_to_request endpoint routes in
  assert (request.method_ = `GET);
  let path = Uri.(to_string @@ make ~path:request.path ~query:(MQ.to_strings request.query) ()) in
  (* FIXME: a bit stupid to convert it to string, we should just carry [Uri.t] around! *)
  if api_prefix then "/" ^ Constant.api_prefix ^ path else path

let path_versionLy slug = path @@ version @@ VersionEndpoints.ly slug
let path_versionSvg ?params slug = path @@ version @@ VersionEndpoints.svg slug params
let path_versionOgg slug = path @@ version @@ VersionEndpoints.ogg slug
let path_versionPdf ?params slug = path @@ version @@ VersionEndpoints.pdf slug params
let path_setPdf ?params slug = path @@ set @@ SetEndpoints.pdf slug params
let path_bookPdf ?params slug = path @@ book @@ BookEndpoints.pdf slug params

let endpoint method_ path query =
  Madge_router.request_to_resource { method_; path; query } routes
