open Dancelor_common_model

(** Existing endpoints in Dancelor's API. *)
type endpoint =
  | Book of BookEndpoints.t
  | Set of SetEndpoints.t
  | Version of VersionEndpoints.t
  | Victor
  | Victor2
  | Victor3
  | Victor4

let mkBook e = Book e
let unBook = function Book e -> Some e | _ -> None

let mkSet e = Set e
let unSet = function Set e -> Some e | _ -> None

let mkVersion e = Version e
let unVersion = function Version e -> Some e | _ -> None

(** Constructors that can be used as functions. FIXME: This is a job for a PPX
    and there is probably one that exists for that. *)
let bookPdf slug params = Book (Pdf (slug, params))
let setLy slug params = Set (Ly (slug, params))
let setPdf slug params = Set (Pdf (slug, params))
let versionLy slug = Version (Ly slug)
let versionSvg slug params = Version (Svg (slug, params))
let versionOgg slug = Version (Ogg slug)
let versionPdf slug params = Version (Pdf (slug, params))

(** {2 Routes} *)

open Madge_router
module MQ = Madge_query

let routes : endpoint route list =
  wrap_routes ~wrap:mkBook ~unwrap:unBook BookEndpoints.routes
  @ wrap_routes ~wrap:mkSet ~unwrap:unSet SetEndpoints.routes
  @ wrap_routes ~wrap:mkVersion ~unwrap:unVersion VersionEndpoints.routes
  @ [ direct `GET "/victor"  Victor ;
      direct `GET "/victor2" Victor2 ;
      direct `GET "/victor3" Victor3 ;
      direct `GET "/victor4" Victor4 ]

let path ?(api_prefix=true) endpoint =
  let request = Madge_router.resource_to_request endpoint routes in
  assert (request.method_ = `GET);
  let path = Uri.(to_string @@ make ~path:request.path ~query:(MQ.to_strings request.query) ()) in
  (* FIXME: a bit stupid to convert it to string, we should just carry [Uri.t] around! *)
  if api_prefix then "/" ^ Constant.api_prefix ^ path else path

let endpoint method_ path query =
  Madge_router.request_to_resource { method_; path; query } routes
