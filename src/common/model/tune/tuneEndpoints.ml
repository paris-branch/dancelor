open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(TuneCore))
  let status = optarg (module Status)
  let filter = arg (module TuneFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let name = arg (module MString)
  let alternative_names = optarg (module MList(MString))
  let kind = arg (module Kind.Base)
  let author = optarg (module CreditCore)
  let dances = optarg (module MList(DanceCore))
  let remark = optarg (module MString)
  let scddb_id = optarg (module MInteger)
end

let get = endpoint ~path:"/tune" (module TuneCore)
let make_and_save = endpoint ~path:"/tune/save" (module TuneCore)
let search = endpoint ~path:"/tune/search" (module MList(Score.Make_Serialisable(TuneCore)))
