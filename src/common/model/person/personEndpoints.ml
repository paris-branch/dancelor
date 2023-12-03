open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(PersonCore))
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let scddb_id = optarg ~key:"scddb_id" (module MInteger)
  let pagination = optarg (module Pagination)
  let filter = arg (module PersonCore.Filter)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/person" (module PersonCore)
let make_and_save = endpoint ~path:"/person/save" (module PersonCore)
let search = endpoint ~path:"/person/search" (module MPair (MInteger) (MList(Score.Make_Serialisable(PersonCore))))
