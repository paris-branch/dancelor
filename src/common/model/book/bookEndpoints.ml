open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((BookCore.t Slug.t -> 'w), 'w, BookCore.t Entry.t) t
  | Search : ((Slice.t -> BookCore.Filter.t -> 'w), 'w, (int * BookCore.t Entry.t list)) t
  | Create : ((BookCore.t -> 'w), 'w, BookCore.t Entry.t) t
  | Update : ((BookCore.t Slug.t -> BookCore.t -> 'w), 'w, BookCore.t Entry.t) t
  | Pdf : ((BookParameters.t -> BookCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(BookCore)) @@ return (module Entry.J(BookCore))
  | Pdf -> literal "pdf" @@ query "parameters" (module BookParameters) @@ variable (module SSlug(BookCore)) @@ return (module JVoid)
  | Search -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module BookCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(BookCore))))
  | Create -> literal "create" @@ query "book" (module BookCore) @@ return (module Entry.J(BookCore))
  | Update -> literal "update" @@ variable (module SSlug(BookCore)) @@ query "book" (module BookCore) @@ return (module Entry.J(BookCore))
