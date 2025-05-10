open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without a specific dance *)
| Create : ((Core.Dance.t -> 'w), 'w, Core.Dance.t Entry.t) t
| Search : ((Slice.t -> Filter.Dance.t -> 'w), 'w, (int * Core.Dance.t Entry.t list)) t
(* Actions on a specific dance *)
| Get : ((Core.Dance.t Slug.t -> 'w), 'w, Core.Dance.t Entry.t) t
| Update : ((Core.Dance.t Slug.t -> Core.Dance.t -> 'w), 'w, Core.Dance.t Entry.t) t
(* Files related to a dance *)
| Pdf : ((Core.Dance.t Slug.t -> Core.SetParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without a specific dance *)
    | Create -> body "dance" (module Core.Dance) @@ post (module Entry.J(Core.Dance))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Dance) @@ get (module JPair(JInt)(JList(Entry.J(Core.Dance))))
    (* Actions on a specific dance *)
    | Get -> variable (module SSlug(Core.Dance)) @@ get (module Entry.J(Core.Dance))
    | Update -> variable (module SSlug(Core.Dance)) @@ body "dance" (module Core.Dance) @@ put (module Entry.J(Core.Dance))
    (* Files related to a dance *)
    | Pdf -> variable (module SSlug(Core.Dance)) ~suffix: ".pdf" @@ query "parameters" (module Core.SetParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
