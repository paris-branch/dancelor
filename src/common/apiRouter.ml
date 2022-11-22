open Nes
open Dancelor_common_model

(** Existing endpoints in Dancelor's API. *)
type endpoint =
  | Index
  | MagicSearch (* FIXME: argument *)

  | CreditSave
  | Credit of CreditCore.t Slug.t

  | Dance of DanceCore.t Slug.t

  | PersonSave
  | Person of PersonCore.t Slug.t

  | BookAll
  | BookCompose
  | BookPdf of BookCore.t Slug.t * BookParameters.t option
  | Book of BookCore.t Slug.t

  | SetAll
  | SetCompose
  | SetSave
  | SetLy of SetCore.t Slug.t
  | SetPdf of SetCore.t Slug.t * SetParameters.t option
  | Set of SetCore.t Slug.t
  | SetDelete of SetCore.t Slug.t

  | Tune of TuneCore.t Slug.t

  | VersionAddition
  | VersionAll
  | VersionSearch
  | VersionLy of VersionCore.t Slug.t
  | VersionSvg of VersionCore.t Slug.t
  | VersionOgg of VersionCore.t Slug.t
  | VersionPdf of VersionCore.t Slug.t * VersionParameters.t option
  | Version of VersionCore.t Slug.t

  | Victor

(** Constructors that can be used as functions. FIXME: This is a job for a PPX
    and there is probably one that exists for that. *)
let credit slug = Credit slug
let dance slug = Dance slug
let person slug = Person slug
let bookPdf slug params = BookPdf (slug, params)
let book slug = Book slug
let setLy slug = SetLy slug
let setPdf slug params = SetPdf (slug, params)
let set slug = Set slug
let setDelete slug = SetDelete slug
let tune slug = Tune slug
let versionLy slug = VersionLy slug
let versionSvg slug = VersionSvg slug
let versionOgg slug = VersionOgg slug
let versionPdf slug params = VersionPdf (slug, params)
let version slug = Version slug

(** Destructors. FIXME: This is also a job for a PPX. *)
let unCredit = function Credit slug -> Some slug | _ -> None
let unDance = function Dance slug -> Some slug | _ -> None
let unPerson = function Person slug -> Some slug | _ -> None
let unBookPdf = function BookPdf (slug, params) -> Some (slug, params) | _ -> None
let unBook = function Book slug -> Some slug | _ -> None
let unSetLy = function SetLy slug -> Some slug | _ -> None
let unSetPdf = function SetPdf (slug, params) -> Some (slug, params) | _ -> None
let unSet = function Set slug -> Some slug | _ -> None
let unSetDelete = function SetDelete slug -> Some slug | _ -> None
let unTune = function Tune slug -> Some slug | _ -> None
let unVersionLy = function VersionLy slug -> Some slug | _ -> None
let unVersionSvg = function VersionSvg slug -> Some slug | _ -> None
let unVersionOgg = function VersionOgg slug -> Some slug | _ -> None
let unVersionPdf = function VersionPdf (slug, params) -> Some (slug, params) | _ -> None
let unVersion = function Version slug -> Some slug | _ -> None

(** {2 Routes} *)

let routes : endpoint Madge_router.route list = let open Madge_router in
  [
    direct      `GET  "/"                    Index ;
    direct      `GET  "/search"              MagicSearch ;
    direct      `GET  "/credit/save"         CreditSave ;
    with_slug   `GET  "/credit"             (credit, unCredit) ;
    with_slug   `GET  "/dance"              (dance, unDance) ;
    direct      `GET  "/person/save"         PersonSave ;
    with_slug   `GET  "/person"             (person, unPerson) ;
    direct      `GET  "/book/all"            BookAll ;
    direct      `GET  "/book/compose"        BookCompose ;
    with_slug   `GET  "/book" ~ext:"pdf"    ((fun slug -> BookPdf (slug, None)), (function BookPdf (slug, _) -> Some slug | _ -> None)) ; (* FIXME *)
    with_slug   `GET  "/book"               (book, unBook) ;
    direct      `GET  "/set/all"             SetAll ;
    direct      `GET  "/set/compose"         SetCompose ;
    direct      `GET  "/set/save"            SetSave ;
    with_slug   `GET  "/set" ~ext:"ly"      (setLy, unSetLy) ;
    with_slug   `GET  "/set" ~ext:"pdf"     ((fun slug -> SetPdf (slug, None)), (function SetPdf (slug, _) -> Some slug | _ -> None)) ; (* FIXME *)
    with_slug   `GET  "/set"                (set, unSet) ;
    with_slug `DELETE "/set"                (setDelete, unSetDelete) ;
    with_slug   `GET  "/tune"               (tune, unTune) ;
    direct      `GET  "/version/add"         VersionAddition ;
    direct      `GET  "/version/all"         VersionAll ;
    direct      `GET  "/version/search"      VersionSearch ;
    with_slug   `GET  "/version" ~ext:"ly"  (versionLy, unVersionLy) ;
    with_slug   `GET  "/version" ~ext:"svg" (versionSvg, unVersionSvg) ;
    with_slug   `GET  "/version" ~ext:"ogg" (versionOgg, unVersionOgg) ;
    with_slug   `GET  "/version" ~ext:"pdf" ((fun slug -> VersionPdf (slug, None)), (function VersionPdf (slug, _) -> Some slug | _ -> None)) ; (* FIXME *)
    with_slug   `GET  "/version"            (version,    unVersion) ;
    direct      `GET  "/victor"              Victor ;
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
