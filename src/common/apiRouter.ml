open Nes
open Dancelor_common_model

(** Existing endpoints in Dancelor's API. *)
type endpoint =
  | BookPdf of BookCore.t Slug.t * BookParameters.t option
  | SetLy of SetCore.t Slug.t
  | SetPdf of SetCore.t Slug.t * SetParameters.t option
  | VersionLy of VersionCore.t Slug.t
  | VersionSvg of VersionCore.t Slug.t
  | VersionOgg of VersionCore.t Slug.t
  | VersionPdf of VersionCore.t Slug.t * VersionParameters.t option
  | Victor
  | Victor2
  | Victor3
  | Victor4

(** Constructors that can be used as functions. FIXME: This is a job for a PPX
    and there is probably one that exists for that. *)
let bookPdf slug params = BookPdf (slug, params)
let setLy slug = SetLy slug
let setPdf slug params = SetPdf (slug, params)
let versionLy slug = VersionLy slug
let versionSvg slug = VersionSvg slug
let versionOgg slug = VersionOgg slug
let versionPdf slug params = VersionPdf (slug, params)

(** Destructors. FIXME: This is also a job for a PPX. *)
let unBookPdf = function BookPdf (slug, params) -> Some (slug, params) | _ -> None
let unSetLy = function SetLy slug -> Some slug | _ -> None
let unSetPdf = function SetPdf (slug, params) -> Some (slug, params) | _ -> None
let unVersionLy = function VersionLy slug -> Some slug | _ -> None
let unVersionSvg = function VersionSvg slug -> Some slug | _ -> None
let unVersionOgg = function VersionOgg slug -> Some slug | _ -> None
let unVersionPdf = function VersionPdf (slug, params) -> Some (slug, params) | _ -> None

(** {2 Routes} *)

let routes : endpoint Madge_router.route list = let open Madge_router in
  [
    with_slug   `GET  "/book" ~ext:"pdf"    ((fun slug -> BookPdf (slug, None)), (function BookPdf (slug, _) -> Some slug | _ -> None)) ; (* FIXME *)
    with_slug   `GET  "/set" ~ext:"ly"      (setLy, unSetLy) ;
    with_slug   `GET  "/set" ~ext:"pdf"     ((fun slug -> SetPdf (slug, None)), (function SetPdf (slug, _) -> Some slug | _ -> None)) ; (* FIXME *)
    with_slug   `GET  "/version" ~ext:"ly"  (versionLy, unVersionLy) ;
    with_slug   `GET  "/version" ~ext:"svg" (versionSvg, unVersionSvg) ;
    with_slug   `GET  "/version" ~ext:"ogg" (versionOgg, unVersionOgg) ;
    with_slug   `GET  "/version" ~ext:"pdf" ((fun slug -> VersionPdf (slug, None)), (function VersionPdf (slug, _) -> Some slug | _ -> None)) ; (* FIXME *)
    direct      `GET  "/victor"              Victor ;
    direct      `GET  "/victor2"             Victor2 ;
    direct      `GET  "/victor3"             Victor3 ;
    direct      `GET  "/victor4"             Victor4 ;
  ]

(* let path_to_endpoint ~meth ~path = Madge_router.path_to_resource meth path routes *)

(* let path_of_endpoint ~api_prefix resource = *)
(*   let (method_, path) = Madge_router.resource_to_path resource routes in *)
(*   if api_prefix *)
(*   then (method_, Constant.api_prefix ^ "/" ^ path) *)
(*   else (method_, path) *)

(* let path_of_get_endpoint ~api_prefix resource = *)
(*   let (meth, path) = path_of_endpoint ~api_prefix resource in *)
(*   if not (meth = `GET) then *)
(*     failwith "path_of_get_resource"; *)
(*   path *)

(* let gpath ~api resource = *)
(*   path_of_get_endpoint ~api_prefix:api resource *)
