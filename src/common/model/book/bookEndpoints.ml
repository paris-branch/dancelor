open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((BookCore.t Slug.t -> 'w), 'w, BookCore.t Entry.t) t
  | Search : ((Slice.t option -> float option -> BookCore.Filter.t -> 'w), 'w, (int * BookCore.t Entry.t list)) t
  | Save : ((Status.t option -> Datetime.t -> Datetime.t -> BookCore.t -> 'w), 'w, BookCore.t Entry.t) t
  | Update : ((Status.t option -> Datetime.t -> Datetime.t -> BookCore.t Slug.t -> BookCore.t -> 'w), 'w, unit) t
  | Pdf : ((BookParameters.t option -> BookCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(BookCore)) @@ return (module Entry.J(BookCore))
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module BookParameters) @@ variable (module SSlug(BookCore)) @@ return (module JVoid)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module BookCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(BookCore))))
  | Save -> literal "save" @@ query_opt "status" (module Status) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ query "book" (module BookCore) @@ return (module Entry.J(BookCore))
  | Update -> literal "update" @@ query_opt "status" (module Status) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ variable (module SSlug(BookCore)) @@ query "book" (module BookCore) @@ return (module JUnit)
