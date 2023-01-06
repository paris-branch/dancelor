open Nes
open Dancelor_common_model

(** Existing endpoints in Dancelor's API. *)
type endpoint =
  | Book of BookEndpoints.t
  | Set of SetEndpoints.t
  | VersionLy of VersionCore.t Slug.t
  | VersionSvg of VersionCore.t Slug.t * VersionParameters.t option
  | VersionOgg of VersionCore.t Slug.t
  | VersionPdf of VersionCore.t Slug.t * VersionParameters.t option
  | Victor
  | Victor2
  | Victor3
  | Victor4

let book e = Book e
let unBook = function Book e -> Some e | _ -> None

let set e = Set e
let unSet = function Set e -> Some e | _ -> None

(** Constructors that can be used as functions. FIXME: This is a job for a PPX
    and there is probably one that exists for that. *)
let bookPdf slug params = Book (Pdf (slug, params))
let setLy slug params = Set (Ly (slug, params))
let setPdf slug params = Set (Pdf (slug, params))
let versionLy slug = VersionLy slug
let versionSvg slug params = VersionSvg (slug, params)
let versionOgg slug = VersionOgg slug
let versionPdf slug params = VersionPdf (slug, params)

(** Destructors. FIXME: This is also a job for a PPX. *)
let unBookPdf = function Book (Pdf (slug, params)) -> Some (slug, params) | _ -> None
let unSetLy = function Set (Ly (slug, params)) -> Some (slug, params) | _ -> None
let unSetPdf = function Set (Pdf (slug, params)) -> Some (slug, params) | _ -> None
let unVersionLy = function VersionLy slug -> Some slug | _ -> None
let unVersionSvg = function VersionSvg (slug, params) -> Some (slug, params) | _ -> None
let unVersionOgg = function VersionOgg slug -> Some slug | _ -> None
let unVersionPdf = function VersionPdf (slug, params) -> Some (slug, params) | _ -> None

(** {2 Routes} *)

open Madge_router
module MQ = Madge_query

let routes : endpoint route list =
  wrap_routes ~wrap:book ~unwrap:unBook BookEndpoints.routes
  @ wrap_routes ~wrap:set ~unwrap:unSet SetEndpoints.routes
  @ [

    with_slug   `GET  "/version" ~ext:"ly"  (versionLy, unVersionLy) ;

    with_slug_and_query `GET "/version" ~ext:"svg"
      (fun slug query -> VersionSvg (slug, MQ.get_ "parameters" VersionParameters.of_yojson query))
      (function
        | VersionSvg (slug, None) -> Some (slug, MQ.empty)
        | VersionSvg (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ VersionParameters.to_yojson params)
        | _ -> None) ;

    with_slug   `GET  "/version" ~ext:"ogg" (versionOgg, unVersionOgg) ;

    with_slug_and_query `GET "/version" ~ext:"pdf"
      (fun slug query -> VersionPdf (slug, MQ.get_ "parameters" VersionParameters.of_yojson query))
      (function
        | VersionPdf (slug, None) -> Some (slug, MQ.empty)
        | VersionPdf (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ VersionParameters.to_yojson params)
        | _ -> None) ;

    direct      `GET  "/victor"              Victor ;
    direct      `GET  "/victor2"             Victor2 ;
    direct      `GET  "/victor3"             Victor3 ;
    direct      `GET  "/victor4"             Victor4 ;
  ]

let path ?(api_prefix=true) endpoint =
  let request = Madge_router.resource_to_request endpoint routes in
  assert (request.method_ = `GET);
  let path = Uri.(to_string @@ make ~path:request.path ~query:(MQ.to_strings request.query) ()) in
  (* FIXME: a bit stupid to convert it to string, we should just carry [Uri.t] around! *)
  if api_prefix then "/" ^ Constant.api_prefix ^ path else path

let endpoint method_ path query =
  Madge_router.request_to_resource { method_; path; query } routes
