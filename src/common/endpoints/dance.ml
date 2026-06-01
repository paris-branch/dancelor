open Nes
open Madge
open Model_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
(* Actions without a specific dance *)
| Create : ((Dance.t -> 'w), 'w, Dance.entry) t
| Search : ((Slice.t -> (Dance.t, Filter.Dance.t) Formula_entry.public -> 'w), 'w, Dance.entry search_result) t
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
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPublic(Dance)(Filter.Dance)) @@ get (module Utils.Search_result(Entry.JPublic(Dance)))
    (* Actions on a specific dance *)
    | Get -> variable (module Entry.Id.S(Dance)) @@ get (module Entry.JPublic(Dance))
    | Update -> variable (module Entry.Id.S(Dance)) @@ body "dance" (module Dance) @@ put (module Entry.JPublic(Dance))
    | Delete -> variable (module Entry.Id.S(Dance)) @@ delete (module JUnit)
