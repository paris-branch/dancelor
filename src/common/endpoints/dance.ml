open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
(* Actions without a specific dance *)
| Create : ((Dance.t -> 'w), 'w, Dance.t Entry.t) t
| Search : ((Slice.t -> Filter.Dance.t -> 'w), 'w, (int * Dance.t Entry.t list)) t
(* Actions on a specific dance *)
| Get : ((Dance.t Entry.Id.t -> 'w), 'w, Dance.t Entry.t) t
| Update : ((Dance.t Entry.Id.t -> Dance.t -> 'w), 'w, Dance.t Entry.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without a specific dance *)
    | Create -> body "dance" (module Dance) @@ post (module Entry.J(Dance))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Dance) @@ get (module JPair(JInt)(JList(Entry.J(Dance))))
    (* Actions on a specific dance *)
    | Get -> variable (module Entry.Id.S(Dance)) @@ get (module Entry.J(Dance))
    | Update -> variable (module Entry.Id.S(Dance)) @@ body "dance" (module Dance) @@ put (module Entry.J(Dance))
