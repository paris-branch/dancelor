open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(TuneCore))
  let status = optarg (module Status)
  let filter = arg (module TuneFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/tune" (module TuneCore)
let search = endpoint ~path:"/tune/search" (module MList(Score.Make_Serialisable(TuneCore)))
