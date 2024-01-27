open Madge_common

module Arguments = struct
  let filter = arg (module AnyCore.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let element = arg (module AnyCore)
end

let search = endpoint ~path:"/any/search" (module MPair (MInteger) (MList (Score.Make_Serialisable (AnyCore))))
let search_context = endpoint ~path:"/any/search-context" (module MQuadruple (MInteger) (MOption(AnyCore)) (MInteger) (MOption(AnyCore)))
