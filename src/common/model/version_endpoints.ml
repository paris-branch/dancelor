open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(VersionCore))
  let status = optarg (module Status)
  let filter = arg (module VersionFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/version" (module VersionCore)
let search = endpoint ~path:"/version/search" (module MList(Score.Make_Serialisable(VersionCore)))
let count = endpoint ~path:"/version/count" (module MInteger)
