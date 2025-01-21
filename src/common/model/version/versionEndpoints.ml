open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((VersionCore.t Slug.t -> 'w), 'w, VersionCore.t Entry.t) t
  | Search : ((Slice.t option -> float option -> VersionCore.Filter.t -> 'w), 'w, (int * VersionCore.t Entry.t list)) t
  | Save : ((VersionCore.t -> 'w), 'w, VersionCore.t Entry.t) t
  | Ly : ((VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Svg : ((VersionParameters.t option -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Ogg : ((VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Pdf : ((VersionParameters.t option -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W Ly; W Svg; W Ogg; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(VersionCore)) @@ return (module Entry.J(VersionCore))
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module VersionCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(VersionCore))))
  | Save -> literal "save" @@ query "version" (module VersionCore) @@ return (module Entry.J(VersionCore))
  | Ly -> literal "ly" @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Svg -> literal "svg" @@ query_opt "parameters" (module VersionParameters) @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Ogg -> literal "ogg" @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module VersionParameters) @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
