open Nes
open Madge
open Madge_common

type (_, _, _) t =
  | Get : ((BookCore.t Slug.t -> 'w), 'w, BookCore.t) t
  | Search : ((Slice.t option -> float option -> BookCore.Filter.t -> 'w), 'w, (int * BookCore.t list)) t
  | Save : ((Status.t option -> string -> PartialDate.t option -> BookCore.PageCore.t list option -> Datetime.t -> Datetime.t -> 'w), 'w, BookCore.t) t
  | Update : ((Status.t option -> string -> PartialDate.t option -> BookCore.PageCore.t list option -> Datetime.t -> Datetime.t -> BookCore.t Slug.t -> 'w), 'w, unit) t
  | Pdf : ((BookParameters.t option -> BookCore.t Slug.t -> 'w), 'w, MVoid.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(BookCore)) @@ return (module BookCore)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module BookParameters) @@ variable (module SSlug(BookCore)) @@ return (module JVoid)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module MFloat) @@ query "filter" (module BookCore.Filter) @@ return (module MPair(MInteger)(MList(BookCore)))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "title" (module MString) @@ query_opt "date" (module PartialDate) @@ query_opt "contents" (module MList(BookCore.PageCore)) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module BookCore)
  | Update -> literal "update" @@ query_opt "status" (module Status) @@ query "title" (module MString) @@ query_opt "date" (module PartialDate) @@ query_opt "contents" (module MList(BookCore.PageCore)) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ variable (module SSlug(BookCore)) @@ return (module MUnit)
