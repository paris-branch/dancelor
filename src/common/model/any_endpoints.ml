open Madge_common

module Arguments = struct
  let filter = arg (module Any.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let search = endpoint ~path:"/any/search" (module MList (Score.Make_Serialisable (Any)))
