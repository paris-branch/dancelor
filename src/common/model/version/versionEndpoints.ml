open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((VersionCore.t Slug.t -> 'w), 'w, VersionCore.t Entry.t) t
  | Search : ((Slice.t -> VersionCore.Filter.t -> 'w), 'w, (int * VersionCore.t Entry.t list)) t
  | Create : ((VersionCore.t -> 'w), 'w, VersionCore.t Entry.t) t
  | Update : ((VersionCore.t Slug.t -> VersionCore.t -> 'w), 'w, VersionCore.t Entry.t) t
  | Ly : ((VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Svg : ((VersionParameters.t -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Ogg : ((VersionParameters.t -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t
  | Pdf : ((VersionParameters.t -> VersionCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Ly; W Svg; W Ogg; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(VersionCore)) @@ return (module Entry.J(VersionCore))
  | Search -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module VersionCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(VersionCore))))
  | Create -> literal "create" @@ query "version" (module VersionCore) @@ return (module Entry.J(VersionCore))
  | Update -> literal "update" @@ variable (module SSlug(VersionCore)) @@ query "version" (module VersionCore) @@ return (module Entry.J(VersionCore))
  | Ly -> literal "ly" @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Svg -> literal "svg" @@ query "parameters" (module VersionParameters) @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Ogg -> literal "ogg" @@ query "parameters" (module VersionParameters) @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
  | Pdf -> literal "pdf" @@ query "parameters" (module VersionParameters) @@ variable (module SSlug(VersionCore)) @@ return (module JVoid)
