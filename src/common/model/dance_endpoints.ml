open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let get = endpoint ~path:"/dance" (module Dance)
let search = endpoint ~path:"/dance/search" (module MList (Score.Make_Serialisable (Dance)))
