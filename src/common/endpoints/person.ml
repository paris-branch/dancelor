open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific person *)
| Create : ((Person.t -> 'w), 'w, Person.t Entry.t) t
| Search : ((Slice.t -> Person.Filter.t -> 'w), 'w, (int * Person.t Entry.t list)) t
(* Actions on a specific person *)
| Get : ((Person.t Slug.t -> 'w), 'w, Person.t Entry.t) t
| Update : ((Person.t Slug.t -> Person.t -> 'w), 'w, Person.t Entry.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific person *)
    | Create -> body "person" (module Person) @@ post (module Entry.J(Person))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Person.Filter) @@ get (module JPair(JInt)(JList(Entry.J(Person))))
    (* Actions on a specific person *)
    | Get -> variable (module SSlug(Person)) @@ get (module Entry.J(Person))
    | Update -> variable (module SSlug(Person)) @@ body "person" (module Person) @@ put (module Entry.J(Person))
