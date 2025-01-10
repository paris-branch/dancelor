open Nes
open Madge
open Madge_common

type (_, _, _) t =
  | Get : ((VersionCore.t Slug.t -> 'w), 'w, VersionCore.t) t
  | Search : ((Slice.t option -> float option -> VersionCore.Filter.t -> 'w), 'w, (int * VersionCore.t list)) t
  | Save : ((Status.t option -> TuneCore.t -> int -> Music.key -> string -> PersonCore.t list option -> string option -> string option -> bool option -> string -> Datetime.t -> Datetime.t -> 'w), 'w, VersionCore.t) t
  | MarkBroken : ((VersionCore.t Slug.t -> 'w), 'w, unit) t
  | MarkFixed : ((VersionCore.t Slug.t -> 'w), 'w, unit) t
  | Ly : ((VersionCore.t Slug.t -> 'w), 'w, MVoid.t) t
  | Svg : ((VersionParameters.t option -> VersionCore.t Slug.t -> 'w), 'w, MVoid.t) t
  | Ogg : ((VersionCore.t Slug.t -> 'w), 'w, MVoid.t) t
  | Pdf : ((VersionParameters.t option -> VersionCore.t Slug.t -> 'w), 'w, MVoid.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W MarkBroken; W MarkFixed; W Ly; W Svg; W Ogg; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(VersionCore)) @@ return (module VersionCore)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module MFloat) @@ query "filter" (module VersionCore.Filter) @@ return (module MPair(MInteger)(MList(VersionCore)))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "tune" (module TuneCore) @@ query "bars" (module MInteger) @@ query "key" (module Music.Key) @@ query "structure" (module MString) @@ query_opt "arrangers" (module MList(PersonCore)) @@ query_opt "remark" (module MString) @@ query_opt "disambiguation" (module MString) @@ query_opt "broken" (module MBool) @@ query "content" (module MString) @@ query "modified_at" (module Datetime) @@ query "created_at" (module Datetime) @@ return (module VersionCore)
  | MarkBroken -> literal "mark-broken" @@ variable (module SSlug(VersionCore)) @@ return (module MUnit)
  | MarkFixed -> literal "mark-fixed" @@ variable (module SSlug(VersionCore)) @@ return (module MUnit)
  | Ly -> literal "ly" @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Svg -> literal "svg" @@ query_opt "parameters" (module VersionParameters) @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Ogg -> literal "ogg" @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module VersionParameters) @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
