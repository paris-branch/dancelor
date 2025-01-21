open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((PersonCore.t Slug.t -> 'w), 'w, PersonCore.t Entry.t) t
  | Search : ((Slice.t option -> float option -> PersonCore.Filter.t -> 'w), 'w, (int * PersonCore.t Entry.t list)) t
  | Create : ((PersonCore.t -> 'w), 'w, PersonCore.t Entry.t) t
  | Update : ((PersonCore.t Slug.t -> PersonCore.t -> 'w), 'w, PersonCore.t Entry.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(PersonCore)) @@ return (module Entry.J(PersonCore))
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module PersonCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(PersonCore))))
  | Create -> literal "create" @@ query "person" (module PersonCore) @@ return (module Entry.J(PersonCore))
  | Update -> literal "update" @@ variable (module SSlug(PersonCore)) @@ query "person" (module PersonCore) @@ return (module Entry.J(PersonCore))
