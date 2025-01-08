open Nes
open Madge_common
open Madge

type (_, _, _) t =
  | Get : ((PersonCore.t Slug.t -> 'w), 'w, PersonCore.t) t
  | Search : ((Slice.t option -> float option -> PersonCore.Filter.t -> 'w), 'w, (int * PersonCore.t list)) t
  | MakeAndSave : ((Status.t option -> string -> int option -> Datetime.t -> Datetime.t -> 'w), 'w, PersonCore.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W MakeAndSave]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(PersonCore)) @@ return (module PersonCore)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module MFloat) @@ query "filter" (module PersonCore.Filter) @@ return (module MPair(MInteger)(MList(PersonCore)))
  | MakeAndSave -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "name" (module MString) @@ query_opt "scddb-id" (module MInteger) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module PersonCore)
