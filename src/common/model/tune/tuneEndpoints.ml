open Nes
open Madge_common
open Madge

type (_, _, _) t =
  | Get : ((TuneCore.t Slug.t -> 'w), 'w, TuneCore.t) t
  | Search : ((Slice.t option -> float option -> TuneCore.Filter.t -> 'w), 'w, (int * TuneCore.t list)) t
  | MakeAndSave : ((Status.t option -> string -> string list option -> Kind.Base.t -> PersonCore.t list option -> DanceCore.t list option -> string option -> int option -> PartialDate.t option -> Datetime.t -> Datetime.t -> 'w), 'w, TuneCore.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W MakeAndSave]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(TuneCore)) @@ return (module TuneCore)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module MFloat) @@ query "filter" (module TuneCore.Filter) @@ return (module MPair(MInteger)(MList(TuneCore)))
  | MakeAndSave -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "name" (module MString) @@ query_opt "alternative-names" (module MList(MString)) @@ query "kind" (module Kind.Base) @@ query_opt "composers" (module MList(PersonCore)) @@ query_opt "dances" (module MList(DanceCore)) @@ query_opt "remark" (module MString) @@ query_opt "scddb-id" (module MInteger) @@ query_opt "date" (module PartialDate) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module TuneCore)
