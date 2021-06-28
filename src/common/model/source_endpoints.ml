open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let get = endpoint ~path:"/source" (module Source)
let make_and_save = endpoint ~path:"/source/save" (module Source)
let search = endpoint ~path:"/source/search" (module MList (Score.Make_Serialisable (Source)))
