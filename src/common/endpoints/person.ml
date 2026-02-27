open Nes
open Madge
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
(* Actions without specific person *)
| Create : ((Person.t -> 'w), 'w, Person.entry) t
| Search : ((Slice.t -> (Person.t, Filter.Person.t) Formula_entry.t -> 'w), 'w, (int * Person.entry list)) t
| For_user : ((User.t Entry.Id.t -> 'w), 'w, Person.entry option) t
(* Actions on a specific person *)
| Get : ((Person.t Entry.Id.t -> 'w), 'w, Person.entry) t
| Update : ((Person.t Entry.Id.t -> Person.t -> 'w), 'w, Person.entry) t
| Delete : ((Person.t Entry.Id.t -> 'w), 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific person *)
    | Create -> body "person" (module Person) @@ post (module Entry.JPublic(Person))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.J(Person)(Filter.Person)) @@ get (module JPair(JInt)(JList(Entry.JPublic(Person))))
    | For_user -> literal "for-user" @@ variable (module Entry.Id.S(User)) @@ get (module JOption(Entry.JPublic(Person)))
    (* Actions on a specific person *)
    | Get -> variable (module Entry.Id.S(Person)) @@ get (module Entry.JPublic(Person))
    | Update -> variable (module Entry.Id.S(Person)) @@ body "person" (module Person) @@ put (module Entry.JPublic(Person))
    | Delete -> variable (module Entry.Id.S(Person)) @@ delete (module JUnit)
