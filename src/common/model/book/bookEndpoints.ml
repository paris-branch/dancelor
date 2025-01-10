open Nes
open Madge

type (_, _, _) t =
  | Get : ((BookCore.t Slug.t -> 'w), 'w, BookCore.t) t
  | Search : ((Slice.t option -> float option -> BookCore.Filter.t -> 'w), 'w, (int * BookCore.t list)) t
  | Save : ((Status.t option -> string -> PartialDate.t option -> BookCore.PageCore.t list option -> Datetime.t -> Datetime.t -> 'w), 'w, BookCore.t) t
  | Update : ((Status.t option -> string -> PartialDate.t option -> BookCore.PageCore.t list option -> Datetime.t -> Datetime.t -> BookCore.t Slug.t -> 'w), 'w, unit) t
  | Pdf : ((BookParameters.t option -> BookCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(BookCore)) @@ return (module BookCore)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module BookParameters) @@ variable (module SSlug(BookCore)) @@ return (module JVoid)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module BookCore.Filter) @@ return (module JPair(JInt)(JList(BookCore)))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "title" (module JString) @@ query_opt "date" (module PartialDate) @@ query_opt "contents" (module JList(BookCore.PageCore)) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module BookCore)
  | Update -> literal "update" @@ query_opt "status" (module Status) @@ query "title" (module JString) @@ query_opt "date" (module PartialDate) @@ query_opt "contents" (module JList(BookCore.PageCore)) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ variable (module SSlug(BookCore)) @@ return (module JUnit)
