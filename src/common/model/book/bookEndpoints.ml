open Madge_common

module Page_slug : Madge_common.SERIALISABLE
  with type t = BookCore.page_slug
  = struct
  type t = BookCore.page_slug
  let _key = "book-page-slug"
  let to_yojson = BookCore.page_slug_to_yojson
  let of_yojson = BookCore.page_slug_of_yojson
end

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(BookCore))
  let status = optarg (module Status)
  let title = arg ~key:"title" (module MString)
  let contents_and_parameters = optarg ~key:"contents" (module MList(Page_slug))
  let filter = arg (module BookFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
end

let get = endpoint ~path:"/book" (module BookCore)
let make_and_save = endpoint ~path:"/book/save" (module BookCore)
let search = endpoint ~path:"/book/search" (module MList(Score.Make_Serialisable(BookCore)))
