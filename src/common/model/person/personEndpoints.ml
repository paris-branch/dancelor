open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((PersonCore.t Slug.t -> 'w), 'w, PersonCore.t) t
  | Search : ((Slice.t option -> float option -> PersonCore.Filter.t -> 'w), 'w, (int * PersonCore.t list)) t
  | Save : ((Status.t option -> string -> int option -> Datetime.t -> Datetime.t -> 'w), 'w, PersonCore.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(PersonCore)) @@ return (module PersonCore)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module PersonCore.Filter) @@ return (module JPair(JInt)(JList(PersonCore)))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "name" (module JString) @@ query_opt "scddb-id" (module JInt) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module PersonCore)
