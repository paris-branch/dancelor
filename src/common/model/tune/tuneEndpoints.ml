open Nes
open Madge

type (_, _, _) t =
  | Get : ((TuneCore.t Slug.t -> 'w), 'w, TuneCore.t) t
  | Search : ((Slice.t option -> float option -> TuneCore.Filter.t -> 'w), 'w, (int * TuneCore.t list)) t
  | Save : ((Status.t option -> string -> string list option -> Kind.Base.t -> PersonCore.t list option -> DanceCore.t list option -> string option -> int option -> PartialDate.t option -> Datetime.t -> Datetime.t -> 'w), 'w, TuneCore.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(TuneCore)) @@ return (module TuneCore)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module TuneCore.Filter) @@ return (module JPair(JInt)(JList(TuneCore)))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "name" (module JString) @@ query_opt "alternative-names" (module JList(JString)) @@ query "kind" (module Kind.Base) @@ query_opt "composers" (module JList(PersonCore)) @@ query_opt "dances" (module JList(DanceCore)) @@ query_opt "remark" (module JString) @@ query_opt "scddb-id" (module JInt) @@ query_opt "date" (module PartialDate) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module TuneCore)
