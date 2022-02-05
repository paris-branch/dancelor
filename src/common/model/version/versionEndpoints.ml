open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(VersionCore))
  let version = arg (module VersionCore)
  let status = optarg (module Status)
  let filter = arg (module VersionFilter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)

  let tune = arg (module TuneCore)
  let bars = arg (module MInteger)
  let key = arg (module Music.Key)
  let structure = arg (module MString) (* FIXME: several MString, CANNOT BE GOOD *)
  let arranger = optarg (module CreditCore)
  let remark = optarg (module MString) (* FIXME: several MString, CANNOT BE GOOD*)
  let disambiguation = optarg (module MString) (* FIXME: several MString, CANNOT BE GOOD*)
  let broken = optarg (module MBool)
end

let get = endpoint ~path:"/version" (module VersionCore)
let make_and_save = endpoint ~path:"/version/save" (module VersionCore)
let search = endpoint ~path:"/version/search" (module MList(Score.Make_Serialisable(VersionCore)))
let count = endpoint ~path:"/version/count" (module MInteger)

let mark_fixed = endpoint ~path:"/version/mark-fixed" (module MUnit)
let mark_broken = endpoint ~path:"/version/mark-broken" (module MUnit)
