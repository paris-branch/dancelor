open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific source *)
| Create : ((Source.t -> 'w), 'w, Source.t Entry.t) t
| Search : ((Slice.t -> Source.Filter.t -> 'w), 'w, (int * Source.t Entry.t list)) t
(* Actions on a specific source *)
| Get : ((Source.t Slug.t -> 'w), 'w, Source.t Entry.t) t
| Update : ((Source.t Slug.t -> Source.t -> 'w), 'w, Source.t Entry.t) t
(* Files related to a source *)
| Cover : ((Source.t Slug.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific source *)
    | Create -> body "source" (module Source) @@ post (module Entry.J(Source))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Source.Filter) @@ get (module JPair(JInt)(JList(Entry.J(Source))))
    (* Actions on a specific source *)
    | Get -> variable (module SSlug(Source)) @@ get (module Entry.J(Source))
    | Update -> variable (module SSlug(Source)) @@ body "source" (module Source) @@ put (module Entry.J(Source))
    (* Files related to a source *)
    | Cover -> variable (module SSlug(Source)) ~suffix: ".webp" @@ void ()
