open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(TuneCore))
  let status = optarg (module Status)
  let filter = arg (module TuneFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let name = arg ~key:"name" (module MString)
  let alternative_names = optarg ~key:"alternative-names" (module MList(MString))
  let kind = arg ~key:"kind" (module Kind.Base)
  let author = optarg ~key:"author" (module CreditCore)
  let dances = optarg ~key:"dances" (module MList(DanceCore))
  let remark = optarg ~key:"remark" (module MString)
  let scddb_id = optarg ~key:"scddb-id" (module MInteger)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/tune" (module TuneCore)
let make_and_save = endpoint ~path:"/tune/save" (module TuneCore)
let search = endpoint ~path:"/tune/search" (module MList(Score.Make_Serialisable(TuneCore)))
