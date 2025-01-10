open Nes
open Madge
open Madge_common

type (_, _, _) t =
  | Get : ((SetCore.t Slug.t -> 'w), 'w, SetCore.t) t
  | Search : ((Slice.t option -> float option -> SetCore.Filter.t -> 'w), 'w, (int * SetCore.t list)) t
  | Save : ((Status.t option -> string -> PersonCore.t list option -> Kind.Dance.t -> (VersionCore.t * VersionParameters.t) list option -> SetOrder.t -> DanceCore.t list option -> Datetime.t -> Datetime.t -> 'w), 'w, SetCore.t) t
  | Delete : ((SetCore.t Slug.t -> 'w), 'w, unit) t
  | Pdf : ((SetParameters.t option -> SetCore.t Slug.t -> 'w), 'w, MVoid.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W Delete; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(SetCore)) @@ return (module SetCore)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module MFloat) @@ query "filter" (module SetCore.Filter) @@ return (module MPair(MInteger)(MList(SetCore)))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "name" (module MString) @@ query_opt "conceptors" (module MList(PersonCore)) @@ query "kind" (module Kind.Dance) @@ query_opt "contents" (module MList(MPair(VersionCore)(VersionParameters))) @@ query "order" (module SetOrder) @@ query_opt "dances" (module MList(DanceCore)) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module SetCore)
  | Delete -> literal "update" @@ variable (module SSlug(SetCore)) @@ return (module MUnit)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module SetParameters) @@ variable (module SSlug(SetCore)) @@ return (module JVoid)
