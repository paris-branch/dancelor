open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(BookCore))
  let status = optarg (module Status)
  let title = arg ~key:"title" (module MString)
  let date = optarg ~key:"date" (module NesPartialDate)
  let contents_and_parameters = optarg ~key:"contents" (module MList(BookCore.PageCore))
  let filter = arg (module BookFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/book" (module BookCore)
let make_and_save = endpoint ~path:"/book/save" (module BookCore)
let search = endpoint ~path:"/book/search" (module MList(Score.Make_Serialisable(BookCore)))
let update = endpoint ~path:"/book/update" (module MUnit)
