open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((PersonCore.t Slug.t -> 'w), 'w, PersonCore.t Entry.t) t
  | Search : ((Slice.t -> PersonCore.Filter.t -> 'w), 'w, (int * PersonCore.t Entry.t list)) t
  | Create : ((PersonCore.t -> 'w), 'w, PersonCore.t Entry.t) t
  | Update : ((PersonCore.t Slug.t -> PersonCore.t -> 'w), 'w, PersonCore.t Entry.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(PersonCore)) @@ get (module Entry.J(PersonCore))
  | Search -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module PersonCore.Filter) @@ get (module JPair(JInt)(JList(Entry.J(PersonCore))))
  | Create -> literal "create" @@ query "person" (module PersonCore) @@ get (module Entry.J(PersonCore))
  | Update -> literal "update" @@ variable (module SSlug(PersonCore)) @@ query "person" (module PersonCore) @@ get (module Entry.J(PersonCore))
