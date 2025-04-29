open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific tune *)
| Create : ((Tune.t -> 'w), 'w, Tune.t Entry.t) t
| Search : ((Slice.t -> Tune.Filter.t -> 'w), 'w, (int * Tune.t Entry.t list)) t
(* Actions on a specific tune *)
| Get : ((Tune.t Slug.t -> 'w), 'w, Tune.t Entry.t) t
| Update : ((Tune.t Slug.t -> Tune.t -> 'w), 'w, Tune.t Entry.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific tune *)
    | Create -> body "tune" (module Tune) @@ post (module Entry.J(Tune))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Tune.Filter) @@ get (module JPair(JInt)(JList(Entry.J(Tune))))
    (* Actions on a specific tune *)
    | Get -> variable (module SSlug(Tune)) @@ get (module Entry.J(Tune))
    | Update -> variable (module SSlug(Tune)) @@ body "tune" (module Tune) @@ put (module Entry.J(Tune))
