open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(CreditCore))
  let status = optarg (module Status)
  let line = arg ~key:"line" (module MString)
  let persons = optarg ~key:"persons" (module MList(PersonCore))
  let scddb_id = optarg ~key:"scddb_id" (module MInteger)
  let pagination = optarg (module Pagination)
  let filter = arg (module CreditFilter)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let modified_at = arg ~key:"modified-at" (module NesDate)
end

let get = endpoint ~path:"/credit" (module CreditCore)
let make_and_save = endpoint ~path:"/credit/save" (module CreditCore)
let search = endpoint ~path:"/credit/search" (module MList(Score.Make_Serialisable(CreditCore)))
