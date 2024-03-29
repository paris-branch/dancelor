open Nes
open Madge_common

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(DanceCore))
  let status = optarg (module Status)
  let slice = optarg (module Slice)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let filter = arg (module DanceCore.Filter)
  let name = arg ~key:"name" (module MString)
  let kind = arg ~key:"kind" (module Kind.Dance)
  let devisers = optarg ~key:"deviser" (module MList(PersonCore))
  let disambiguation = optarg ~key:"disambiguation" (module MString)
  let two_chords = arg ~key:"two-chords" (module MBool)
  let scddb_id = optarg ~key:"scddb-id" (module MInteger)
  let date = optarg ~key:"date" (module PartialDate)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/dance" (module DanceCore)
let make_and_save = endpoint ~path:"/dance/save" (module DanceCore)
let search = endpoint ~path:"/dance/search" (module MPair (MInteger) (MList(DanceCore)))
