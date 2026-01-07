open Nes
open Madge
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
(* Actions without specific tune *)
| Create : ((Tune.t -> 'w), 'w, Tune.entry) t
| Search : ((Slice.t -> Filter.Tune.t -> 'w), 'w, (int * Tune.entry list)) t
(* Actions on a specific tune *)
| Get : ((Tune.t Entry.Id.t -> 'w), 'w, Tune.entry) t
| Update : ((Tune.t Entry.Id.t -> Tune.t -> 'w), 'w, Tune.entry) t
| Delete : ((Tune.t Entry.Id.t -> 'w), 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific tune *)
    | Create -> body "tune" (module Tune) @@ post (module Entry.JPublic(Tune))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Tune) @@ get (module JPair(JInt)(JList(Entry.JPublic(Tune))))
    (* Actions on a specific tune *)
    | Get -> variable (module Entry.Id.S(Tune)) @@ get (module Entry.JPublic(Tune))
    | Update -> variable (module Entry.Id.S(Tune)) @@ body "tune" (module Tune) @@ put (module Entry.JPublic(Tune))
    | Delete -> variable (module Entry.Id.S(Tune)) @@ delete (module JUnit)
