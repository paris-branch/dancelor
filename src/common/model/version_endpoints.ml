open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let filter = optarg (module Version.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let get = endpoint ~path:"/version" (module Version)
let all = endpoint ~path:"/version/all" (module MList (Version))
let search = endpoint ~path:"/version/search" (module MList (Score.Make_Serialisable (Version)))
let count = endpoint ~path:"/version/count" (module MInteger)
