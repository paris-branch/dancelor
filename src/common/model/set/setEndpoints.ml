open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((SetCore.t Slug.t -> 'w), 'w, SetCore.t Entry.t) t
  | Search : ((Slice.t option -> float option -> SetCore.Filter.t -> 'w), 'w, (int * SetCore.t Entry.t list)) t
  | Create : ((SetCore.t -> 'w), 'w, SetCore.t Entry.t) t
  | Update : ((SetCore.t Slug.t -> SetCore.t -> 'w), 'w, SetCore.t Entry.t) t
  | Delete : ((SetCore.t Slug.t -> 'w), 'w, unit) t
  | Pdf : ((SetParameters.t option -> SetCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Delete; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(SetCore)) @@ return (module Entry.J(SetCore))
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module SetCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(SetCore))))
  | Create -> literal "create" @@ query "set" (module SetCore) @@ return (module Entry.J(SetCore))
  | Update -> literal "update" @@ variable (module SSlug(SetCore)) @@ query "set" (module SetCore) @@ return (module Entry.J(SetCore))
  | Delete -> literal "delete" @@ variable (module SSlug(SetCore)) @@ return (module JUnit)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module SetParameters) @@ variable (module SSlug(SetCore)) @@ return (module JVoid)
