open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific source *)
| Create : ((Core.Source.t -> 'w), 'w, Core.Source.t Entry.t) t
| Search : ((Slice.t -> Filter.Source.t -> 'w), 'w, (int * Core.Source.t Entry.t list)) t
(* Actions on a specific source *)
| Get : ((Core.Source.t Slug.t -> 'w), 'w, Core.Source.t Entry.t) t
| Update : ((Core.Source.t Slug.t -> Core.Source.t -> 'w), 'w, Core.Source.t Entry.t) t
(* Files related to a source *)
| Cover : ((Core.Source.t Slug.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific source *)
    | Create -> body "source" (module Core.Source) @@ post (module Entry.J(Core.Source))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Source) @@ get (module JPair(JInt)(JList(Entry.J(Core.Source))))
    (* Actions on a specific source *)
    | Get -> variable (module SSlug(Core.Source)) @@ get (module Entry.J(Core.Source))
    | Update -> variable (module SSlug(Core.Source)) @@ body "source" (module Core.Source) @@ put (module Entry.J(Core.Source))
    (* Files related to a source *)
    | Cover -> variable (module SSlug(Core.Source)) ~suffix: ".webp" @@ void ()
