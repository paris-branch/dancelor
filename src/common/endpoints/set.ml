open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
(* Actions without specific set *)
| Create : ((Set.t -> 'w), 'w, Set.t Entry.t) t
| Search : ((Slice.t -> Filter.Set.t -> 'w), 'w, (int * Set.t Entry.t list)) t
(* Actions on a specific set *)
| Get : ((Set.t Entry.Id.t -> 'w), 'w, Set.t Entry.t) t
| Update : ((Set.t Entry.Id.t -> Set.t -> 'w), 'w, Set.t Entry.t) t
| Delete : ((Set.t Entry.Id.t -> 'w), 'w, unit) t
(* Files related to a set *)
| Pdf : ((Set.t Entry.Id.t -> SetParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific set *)
    | Create -> body "set" (module Set) @@ post (module Entry.J(Set))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Set) @@ get (module JPair(JInt)(JList(Entry.J(Set))))
    (* Actions on a specific set *)
    | Get -> variable (module Entry.Id.S(Set)) @@ get (module Entry.J(Set))
    | Update -> variable (module Entry.Id.S(Set)) @@ body "set" (module Set) @@ put (module Entry.J(Set))
    | Delete -> variable (module Entry.Id.S(Set)) @@ delete (module JUnit)
    (* Files related to a set *)
    | Pdf -> variable (module Entry.Id.S(Set)) ~suffix: ".pdf" @@ query "parameters" (module SetParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
