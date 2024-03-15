open Nes
open Madge_common

(* Old-style Endpoints *)
(* FIXME: to be converted into new-style ones. *)

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(SetCore))
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let conceptors = optarg ~key:"conceptors" (module MList(PersonCore))
  let kind = arg ~key:"kind" (module Kind.Dance)
  let versions_and_parameters = optarg ~key:"versions-and-parameters" (module MList(MPair(VersionCore)(VersionParameters)))
  let order = arg (module SetOrder)
  let dances = optarg ~key:"dances" (module MList(DanceCore))
  let filter = arg (module SetCore.Filter)
  let slice = optarg (module Slice)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/set" (module SetCore)
let make_and_save = endpoint ~path:"/set/save" (module SetCore)
let delete = endpoint ~path:"/set/delete" (module MUnit)
let search = endpoint ~path:"/set/search" (module MPair (MInteger) (MList(SetCore)))
let count = endpoint ~path:"/set/count" (module MInteger)

(* New-style Endpoints *)

open Madge_router
module MQ = Madge_query

type t =
  | Pdf of SetCore.t Slug.t * SetParameters.t option

let routes : t route list =
  [
    with_slug_and_query `GET "/" ~ext:"pdf"
      (fun slug query -> Pdf (slug, MQ.get_ "parameters" SetParameters.of_yojson query))
      (function
        | Pdf (slug, None) -> Some (slug, MQ.empty)
        | Pdf (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ SetParameters.to_yojson params))
  ]
