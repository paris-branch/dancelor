open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let name = arg ~key:"name" (module MString)
  let filter = optarg (module Person.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let get = endpoint ~path:"/person" (module Person)
let make_and_save = endpoint ~path:"/person/save" (module Person)
let search = endpoint ~path:"/person/search" (module MList (Score.Make_Serialisable (Person)))