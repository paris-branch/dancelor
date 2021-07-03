open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(Tune))
  let status = optarg (module Status)
  let filter = optarg (module Tune.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let get = endpoint ~path:"/tune" (module Tune)
let all = endpoint ~path:"/tune/all" (module MList(Tune))
let search = endpoint ~path:"/tune/search" (module MList(Score.Make_Serialisable(Tune)))
