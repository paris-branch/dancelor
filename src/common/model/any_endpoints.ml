open Madge_common

module Arguments = struct
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let type_ = optarg ~key:"type" (module MList (Any.Type))
  let string = arg (module MString)
end

let search = endpoint ~path:"/any/search" (module MList (Score.Make_Serialisable (Any)))
