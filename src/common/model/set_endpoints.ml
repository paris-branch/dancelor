open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(Set))
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let deviser = optarg ~key:"deviser" (module Credit)
  let kind = arg ~key:"kind" (module Kind.Dance)
  let versions_and_parameters = optarg ~key:"versions-and-parameters" (module MList(MPair(Version)(VersionParameters)))
  let dances = optarg ~key:"dances" (module MList(Dance))
  let filter = arg (module Set.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/set" (module Set)
let make_and_save = endpoint ~path:"/set/save" (module Set)
let delete = endpoint ~path:"/set/delete" (module MUnit)
let search = endpoint ~path:"/set/search" (module MList(Score.Make_Serialisable(Set)))
let count = endpoint ~path:"/set/count" (module MInteger)
