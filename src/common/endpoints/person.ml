open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
(* Actions without specific person *)
| Create : ((Person.t -> 'w), 'w, Person.t Entry.t) t
| Search : ((Slice.t -> Filter.Person.t -> 'w), 'w, (int * Person.t Entry.t list)) t
(* Actions on a specific person *)
| Get : ((Person.t Entry.Id.t -> 'w), 'w, Person.t Entry.t) t
| Update : ((Person.t Entry.Id.t -> Person.t -> 'w), 'w, Person.t Entry.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific person *)
    | Create -> body "person" (module Person) @@ post (module Entry.J(Person))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Person) @@ get (module JPair(JInt)(JList(Entry.J(Person))))
    (* Actions on a specific person *)
    | Get -> variable (module Entry.Id.S(Person)) @@ get (module Entry.J(Person))
    | Update -> variable (module Entry.Id.S(Person)) @@ body "person" (module Person) @@ put (module Entry.J(Person))
