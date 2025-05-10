open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific tune *)
| Create : ((Core.Tune.t -> 'w), 'w, Core.Tune.t Entry.t) t
| Search : ((Slice.t -> Filter.Tune.t -> 'w), 'w, (int * Core.Tune.t Entry.t list)) t
(* Actions on a specific tune *)
| Get : ((Core.Tune.t Slug.t -> 'w), 'w, Core.Tune.t Entry.t) t
| Update : ((Core.Tune.t Slug.t -> Core.Tune.t -> 'w), 'w, Core.Tune.t Entry.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific tune *)
    | Create -> body "tune" (module Core.Tune) @@ post (module Entry.J(Core.Tune))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Tune) @@ get (module JPair(JInt)(JList(Entry.J(Core.Tune))))
    (* Actions on a specific tune *)
    | Get -> variable (module SSlug(Core.Tune)) @@ get (module Entry.J(Core.Tune))
    | Update -> variable (module SSlug(Core.Tune)) @@ body "tune" (module Core.Tune) @@ put (module Entry.J(Core.Tune))
