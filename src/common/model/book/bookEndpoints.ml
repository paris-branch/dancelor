open Nes
open Madge_common

(* Old-style Endpoints *)
(* FIXME: to be converted into new-style ones. *)

module Arguments = struct
  let slug = arg ~key:"slug" (module MSlug(BookCore))
  let status = optarg (module Status)
  let title = arg ~key:"title" (module MString)
  let date = optarg ~key:"date" (module NesPartialDate)
  let contents = optarg ~key:"contents" (module MList(BookCore.PageCore))
  let filter = arg (module BookCore.Filter)
  let pagination = optarg (module Pagination)
  let threshold = optarg ~key:"threshold" (module MFloat)
  let modified_at = arg ~key:"modified-at" (module NesDatetime)
  let created_at = arg ~key:"created-at" (module NesDatetime)
end

let get = endpoint ~path:"/book" (module BookCore)
let make_and_save = endpoint ~path:"/book/save" (module BookCore)
let search = endpoint ~path:"/book/search" (module MPair (MInteger) (MList(Score.Make_Serialisable(BookCore))))
let update = endpoint ~path:"/book/update" (module MUnit)

(* New-style Endpoints *)

open Madge_router
module MQ = Madge_query

type t =
  | Pdf of BookCore.t Slug.t * BookParameters.t option

let routes : t route list =
  [
    with_slug_and_query `GET "/" ~ext:".pdf"
      (fun slug query -> Pdf (slug, MQ.get_ "parameters" BookParameters.of_yojson query))
      (function
        | Pdf (slug, None) -> Some (slug, MQ.empty)
        | Pdf (slug, Some params) -> Some (slug, MQ.singleton "parameters" @@ BookParameters.to_yojson params));
  ]
