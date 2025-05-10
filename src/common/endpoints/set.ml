open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific set *)
| Create : ((Core.Set.t -> 'w), 'w, Core.Set.t Entry.t) t
| Search : ((Slice.t -> Filter.Set.t -> 'w), 'w, (int * Core.Set.t Entry.t list)) t
(* Actions on a specific set *)
| Get : ((Core.Set.t Slug.t -> 'w), 'w, Core.Set.t Entry.t) t
| Update : ((Core.Set.t Slug.t -> Core.Set.t -> 'w), 'w, Core.Set.t Entry.t) t
| Delete : ((Core.Set.t Slug.t -> 'w), 'w, unit) t
(* Files related to a set *)
| Pdf : ((Core.Set.t Slug.t -> Core.SetParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific set *)
    | Create -> body "set" (module Core.Set) @@ post (module Entry.J(Core.Set))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Set) @@ get (module JPair(JInt)(JList(Entry.J(Core.Set))))
    (* Actions on a specific set *)
    | Get -> variable (module SSlug(Core.Set)) @@ get (module Entry.J(Core.Set))
    | Update -> variable (module SSlug(Core.Set)) @@ body "set" (module Core.Set) @@ put (module Entry.J(Core.Set))
    | Delete -> variable (module SSlug(Core.Set)) @@ delete (module JUnit)
    (* Files related to a set *)
    | Pdf -> variable (module SSlug(Core.Set)) ~suffix: ".pdf" @@ query "parameters" (module Core.SetParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
