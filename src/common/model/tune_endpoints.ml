open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(Tune))
  let status = optarg (module Status)
  let filter = arg (module Tune.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/tune" (module Tune)
let search = endpoint ~path:"/tune/search" (module MList(Score.Make_Serialisable(Tune)))
