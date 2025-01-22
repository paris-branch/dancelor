open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((DanceCore.t Slug.t -> 'w), 'w, DanceCore.t Entry.t) t
  | Search : ((Slice.t -> DanceCore.Filter.t -> 'w), 'w, (int * DanceCore.t Entry.t list)) t
  | Create : ((DanceCore.t -> 'w), 'w, DanceCore.t Entry.t) t
  | Update : ((DanceCore.t Slug.t -> DanceCore.t -> 'w), 'w, DanceCore.t Entry.t) t
  | Pdf : ((SetParameters.t -> DanceCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(DanceCore)) @@ return (module Entry.J(DanceCore))
  | Pdf -> literal "pdf" @@ query "parameters" (module SetParameters) @@ variable (module SSlug(DanceCore)) @@ return (module JVoid)
  | Search -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module DanceCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(DanceCore))))
  | Create -> literal "create" @@ query "dance" (module DanceCore) @@ return (module Entry.J(DanceCore))
  | Update -> literal "update" @@ variable (module SSlug(DanceCore)) @@ query "dance" (module DanceCore) @@ return (module Entry.J(DanceCore))
