open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(DanceCore))
  let status = optarg (module Status)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let filter = arg (module DanceCore.Filter)
  let name = arg ~key:"name" (module MString)
  let kind = arg ~key:"kind" (module Kind.Dance)
  let deviser = optarg ~key:"deviser" (module PersonCore)
  let disambiguation = optarg ~key:"disambiguation" (module MString)
  let two_chords = arg ~key:"two-chords" (module MBool)
  let scddb_id = optarg ~key:"scddb-id" (module MInteger)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/dance" (module DanceCore)
let make_and_save = endpoint ~path:"/dance/save" (module DanceCore)
let search = endpoint ~path:"/dance/search" (module MList(Score.Make_Serialisable(DanceCore)))
