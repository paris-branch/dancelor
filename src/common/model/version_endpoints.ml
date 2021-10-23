open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(Version))
  let status = optarg (module Status)
  let filter = arg (module Version.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/version" (module Version)
let search = endpoint ~path:"/version/search" (module MList(Score.Make_Serialisable(Version)))
let count = endpoint ~path:"/version/count" (module MInteger)
