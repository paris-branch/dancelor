open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MString)
  let status = optarg (module Status)
  let line = arg ~key:"line" (module MString)
  let persons = optarg ~key:"persons" (module MList(Person))
  let pagination = optarg (module Pagination)
  let filter = optarg (module Credit.Filter)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let string = arg (module MString)
end

let get = endpoint ~path:"/credit" (module Credit)
let all = endpoint ~path:"/credit/all" (module MList (Credit))
let make_and_save = endpoint ~path:"/credit/save" (module Credit)
let search = endpoint ~path:"/credit/search" (module MList (Score.Make_Serialisable (Credit)))
