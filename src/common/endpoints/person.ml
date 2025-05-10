open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific person *)
| Create : ((Core.Person.t -> 'w), 'w, Core.Person.t Entry.t) t
| Search : ((Slice.t -> Filter.Person.t -> 'w), 'w, (int * Core.Person.t Entry.t list)) t
(* Actions on a specific person *)
| Get : ((Core.Person.t Slug.t -> 'w), 'w, Core.Person.t Entry.t) t
| Update : ((Core.Person.t Slug.t -> Core.Person.t -> 'w), 'w, Core.Person.t Entry.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific person *)
    | Create -> body "person" (module Core.Person) @@ post (module Entry.J(Core.Person))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Person) @@ get (module JPair(JInt)(JList(Entry.J(Core.Person))))
    (* Actions on a specific person *)
    | Get -> variable (module SSlug(Core.Person)) @@ get (module Entry.J(Core.Person))
    | Update -> variable (module SSlug(Core.Person)) @@ body "person" (module Core.Person) @@ put (module Entry.J(Core.Person))
