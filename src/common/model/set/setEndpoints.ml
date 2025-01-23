open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  (* Actions without specific set *)
  | Create : ((SetCore.t -> 'w), 'w, SetCore.t Entry.t) t
  | Search : ((Slice.t -> SetCore.Filter.t -> 'w), 'w, (int * SetCore.t Entry.t list)) t
  (* Actions on a specific set *)
  | Get : ((SetCore.t Slug.t -> 'w), 'w, SetCore.t Entry.t) t
  | Update : ((SetCore.t Slug.t -> SetCore.t -> 'w), 'w, SetCore.t Entry.t) t
  | Delete : ((SetCore.t Slug.t -> 'w), 'w, unit) t
  (* Files related to a set *)
  | Pdf : ((SetParameters.t -> SetCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Delete; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific set *)
  | Create -> query "set" (module SetCore) @@ post (module Entry.J(SetCore))
  | Search -> query "slice" (module Slice) @@ query "filter" (module SetCore.Filter) @@ get (module JPair(JInt)(JList(Entry.J(SetCore))))
  (* Actions on a specific set *)
  | Get -> variable (module SSlug(SetCore)) @@ get (module Entry.J(SetCore))
  | Update -> variable (module SSlug(SetCore)) @@ query "set" (module SetCore) @@ put (module Entry.J(SetCore))
  | Delete -> variable (module SSlug(SetCore)) @@ delete (module JUnit)
  (* Files related to a set *)
  | Pdf -> query "parameters" (module SetParameters) @@ variable (module SSlug(SetCore)) ~suffix: ".pdf" @@ void ()
