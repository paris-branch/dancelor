open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(BookCore))
  let status = optarg (module Status)
  let title = arg ~key:"title" (module MString)
  let filter = arg (module BookFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/book" (module BookCore)
let make_and_save = endpoint ~path:"/book/save" (module BookCore)
let search = endpoint ~path:"/book/search" (module MList(Score.Make_Serialisable(BookCore)))
