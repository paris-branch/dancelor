open Nes
open Madge
open Model

type (_, _, _) t =
  (* Actions without specific set *)
  | Create : ((Set.t -> 'w), 'w, Set.t Entry.t) t
  | Search : ((Slice.t -> Set.Filter.t -> 'w), 'w, (int * Set.t Entry.t list)) t
  (* Actions on a specific set *)
  | Get : ((Set.t Slug.t -> 'w), 'w, Set.t Entry.t) t
  | Update : ((Set.t Slug.t -> Set.t -> 'w), 'w, Set.t Entry.t) t
  | Delete : ((Set.t Slug.t -> 'w), 'w, unit) t
  (* Files related to a set *)
  | Pdf : ((SetParameters.t -> Set.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Delete; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific set *)
  | Create -> query "set" (module Set) @@ post (module Entry.J(Set))
  | Search -> query "slice" (module Slice) @@ query "filter" (module Set.Filter) @@ get (module JPair(JInt)(JList(Entry.J(Set))))
  (* Actions on a specific set *)
  | Get -> variable (module SSlug(Set)) @@ get (module Entry.J(Set))
  | Update -> variable (module SSlug(Set)) @@ query "set" (module Set) @@ put (module Entry.J(Set))
  | Delete -> variable (module SSlug(Set)) @@ delete (module JUnit)
  (* Files related to a set *)
  | Pdf -> query "parameters" (module SetParameters) @@ variable (module SSlug(Set)) ~suffix: ".pdf" @@ void ()
