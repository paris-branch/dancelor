open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(Book))
  let status = optarg (module Status)
  let filter = arg (module Book.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/book" (module Book)
let search = endpoint ~path:"/book/search" (module MList(Score.Make_Serialisable(Book)))
