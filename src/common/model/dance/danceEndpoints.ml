open Nes
open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(DanceCore))
  let status = optarg (module Status)
  let slice = optarg (module Slice)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let filter = arg (module DanceCore.Filter)
  let name = arg ~key:"name" (module MString)
  let kind = arg ~key:"kind" (module Kind.Dance)
  let devisers = optarg ~key:"deviser" (module MList(PersonCore))
  let disambiguation = optarg ~key:"disambiguation" (module MString)
  let two_chords = optarg ~key:"two-chords" (module MBool)
  let scddb_id = optarg ~key:"scddb-id" (module MInteger)
  let date = optarg ~key:"date" (module PartialDate)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/dance" (module DanceCore)
let make_and_save = endpoint ~path:"/dance/save" (module DanceCore)
let search = endpoint ~path:"/dance/search" (module MPair (MInteger) (MList(DanceCore)))

(* New-style Endpoints *)

open Madge_router
module MQ = Madge_query

type t =
  | Pdf of DanceCore.t Slug.t * SetParameters.t option
[@@deriving variants]

let routes : t route list =
  [
    with_slug_and_query `GET "/" ~ext:"pdf"
      (fun slug query -> Pdf (slug, MQ.get_ "parameters" SetParameters.of_yojson query))
      (function
        | Pdf (slug, None) -> Some (slug, MQ.empty)
        | Pdf (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ SetParameters.to_yojson params))
  ]
