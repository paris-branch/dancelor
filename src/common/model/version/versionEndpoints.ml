open Nes
open Madge_common

(* Old-style Endpoints *)
(* FIXME: to be converted into new-style ones. *)

module Arguments = struct
  let slug = arg ~key: "slug" (module MSlug(VersionCore))
  let version = arg (module VersionCore)
  let status = optarg (module Status)
  let filter = arg (module VersionCore.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key: "threshold" (module MFloat)
  let tune = arg (module TuneCore)
  let bars = arg (module MInteger)
  let key = arg (module Music.Key)
  let structure = arg ~key: "structure" (module MString)
  let arranger = optarg ~key: "arranger" (module CreditCore)
  let remark = optarg ~key: "remark" (module MString)
  let disambiguation = optarg ~key: "disambiguation" (module MString)
  let broken = optarg ~key: "broken" (module MBool)
  let content = arg ~key: "content" (module MString)
  let modified_at = arg ~key: "modified-at" (module NesDatetime)
  let created_at = arg ~key: "created-at" (module NesDatetime)
end

let get = endpoint ~path: "/version" (module VersionCore)
let make_and_save = endpoint ~path: "/version/save" (module VersionCore)
let search = endpoint ~path: "/version/search" (module MList(Score.Make_Serialisable(VersionCore)))
let count = endpoint ~path: "/version/count" (module MInteger)

let mark_fixed = endpoint ~path: "/version/mark-fixed" (module MUnit)
let mark_broken = endpoint ~path: "/version/mark-broken" (module MUnit)

(* New-style Endpoints *)

open Madge_router
module MQ = Madge_query

type t =
  | Ly of VersionCore.t Slug.t
  | Svg of VersionCore.t Slug.t * VersionParameters.t option
  | Ogg of VersionCore.t Slug.t
  | Pdf of VersionCore.t Slug.t * VersionParameters.t option

let routes : t route list =
  [
    with_slug
      `GET
      "/"
      ~ext: "ly"
      (
        (fun slug -> Ly slug),
        (function Ly slug -> Some slug | _ -> None)
      );
    with_slug_and_query
      `GET
      "/"
      ~ext: "svg"
      (fun slug query -> Svg (slug, MQ.get_ "parameters" VersionParameters.of_yojson query))
      (
        function
        | Svg (slug, None) -> Some (slug, MQ.empty)
        | Svg (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ VersionParameters.to_yojson params)
        | _ -> None
      );
    with_slug
      `GET
      "/"
      ~ext: "ogg"
      (
        (fun slug -> Ogg slug),
        (function Ogg slug -> Some slug | _ -> None)
      );
    with_slug_and_query
      `GET
      "/"
      ~ext: "pdf"
      (fun slug query -> Pdf (slug, MQ.get_ "parameters" VersionParameters.of_yojson query))
      (
        function
        | Pdf (slug, None) -> Some (slug, MQ.empty)
        | Pdf (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ VersionParameters.to_yojson params)
        | _ -> None
      );
  ]
