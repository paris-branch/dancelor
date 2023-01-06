open Nes
open Madge_common

(* Old-style Endpoints *)
(* FIXME: to be converted into new-style ones. *)

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(SetCore))
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let deviser = optarg ~key:"deviser" (module CreditCore)
  let kind = arg ~key:"kind" (module Kind.Dance)
  let versions_and_parameters = optarg ~key:"versions-and-parameters" (module MList(MPair(VersionCore)(VersionParameters)))
  let order = arg (module SetOrder)
  let dances = optarg ~key:"dances" (module MList(DanceCore))
  let filter = arg (module SetFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/set" (module SetCore)
let make_and_save = endpoint ~path:"/set/save" (module SetCore)
let delete = endpoint ~path:"/set/delete" (module MUnit)
let search = endpoint ~path:"/set/search" (module MList(Score.Make_Serialisable(SetCore)))
let count = endpoint ~path:"/set/count" (module MInteger)

(* New-style Endpoints *)

open Madge_router
module MQ = Madge_query

type t =
  | Ly of SetCore.t Slug.t * SetParameters.t option
  | Pdf of SetCore.t Slug.t * SetParameters.t option

let routes : t route list =
  [
    with_slug_and_query `GET "/" ~ext:"ly"
      (fun slug query -> Ly (slug, MQ.get_ "parameters" SetParameters.of_yojson query))
      (function
        | Ly (slug, None) -> Some (slug, MQ.empty)
        | Ly (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ SetParameters.to_yojson params)
        | _ -> None) ;

    with_slug_and_query `GET "/" ~ext:"pdf"
      (fun slug query -> Pdf (slug, MQ.get_ "parameters" SetParameters.of_yojson query))
      (function
        | Pdf (slug, None) -> Some (slug, MQ.empty)
        | Pdf (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ SetParameters.to_yojson params)
        | _ -> None) ;
  ]
