open Madge_common

module Arguments = struct
  let filter = optarg (module Formula.Make_Serialisable (Any.Filter))
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let search = endpoint ~path:"/any/search" (module MList (Score.Make_Serialisable (Any)))
