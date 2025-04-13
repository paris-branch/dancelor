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

let to_string : type a w r. (a, w, r) t -> string = function
  | Create -> "Create"
  | Search -> "Search"
  | Get -> "Get"
  | Update -> "Update"

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific person *)
  | Create -> query "person" (module Person) @@ post (module Entry.J(Person))
  | Search -> query "slice" (module Slice) @@ query "filter" (module Person.Filter) @@ get (module JPair(JInt)(JList(Entry.J(Person))))
  (* Actions on a specific person *)
  | Get -> variable (module SSlug(Person)) @@ get (module Entry.J(Person))
  | Update -> variable (module SSlug(Person)) @@ query "person" (module Person) @@ put (module Entry.J(Person))
