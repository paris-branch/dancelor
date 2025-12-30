open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
(* Actions without a specific dance *)
| Create : ((Dance.t -> 'w), 'w, Dance.entry) t
| Search : ((Slice.t -> Filter.Dance.t -> 'w), 'w, (int * Dance.entry list)) t
(* Actions on a specific dance *)
| Get : ((Dance.t Entry.Id.t -> 'w), 'w, Dance.entry) t
| Update : ((Dance.t Entry.Id.t -> Dance.t -> 'w), 'w, Dance.entry) t
| Delete : ((Dance.t Entry.Id.t -> 'w), 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without a specific dance *)
    | Create -> body "dance" (module Dance) @@ post (module Entry.JPublic(Dance))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Dance) @@ get (module JPair(JInt)(JList(Entry.JPublic(Dance))))
    (* Actions on a specific dance *)
    | Get -> variable (module Entry.Id.S(Dance)) @@ get (module Entry.JPublic(Dance))
    | Update -> variable (module Entry.Id.S(Dance)) @@ body "dance" (module Dance) @@ put (module Entry.JPublic(Dance))
    | Delete -> variable (module Entry.Id.S(Dance)) @@ delete (module JUnit)
