open Madge_common

module Arguments = struct
  let filter = arg (module AnyFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let search = endpoint ~path:"/any/search" (module MList (Score.Make_Serialisable (AnyCore)))
let count = endpoint ~path:"/any/count" (module MInteger)
