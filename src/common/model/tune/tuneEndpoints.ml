open Nes
open Madge_common

module Arguments = struct
  let slug = arg ~key: "slug" (module MSlug(TuneCore))
  let status = optarg (module Status)
  let filter = arg (module TuneCore.Filter)
  let slice = optarg (module Slice)
  let threshold = optarg ~key: "threshold" (module MFloat)
  let name = arg ~key: "name" (module MString)
  let alternative_names = optarg ~key: "alternative-names" (module MList(MString))
  let kind = arg ~key: "kind" (module Kind.Base)
  let composers = optarg ~key: "composers" (module MList(PersonCore))
  let dances = optarg ~key: "dances" (module MList(DanceCore))
  let remark = optarg ~key: "remark" (module MString)
  let scddb_id = optarg ~key: "scddb-id" (module MInteger)
  let date = optarg ~key: "date" (module PartialDate)
  let modified_at = arg ~key: "modified-at" (module NesDatetime)
  let created_at = arg ~key: "created-at" (module NesDatetime)
end

let get = endpoint ~path: "/tune" (module TuneCore)
let make_and_save = endpoint ~path: "/tune/save" (module TuneCore)
let search = endpoint ~path: "/tune/search" (module MPair(MInteger)(MList(TuneCore)))
