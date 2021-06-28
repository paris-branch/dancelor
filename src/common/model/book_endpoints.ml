open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let get = endpoint ~path:"/book" (module Book)
let get_all = endpoint ~path:"/book/all" (module MList (Book))
let search = endpoint ~path:"/book/search" (module MList (Score.Make_Serialisable (Book)))
