open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((SetCore.t Slug.t -> 'w), 'w, SetCore.t Entry.t) t
  | Search : ((Slice.t option -> float option -> SetCore.Filter.t -> 'w), 'w, (int * SetCore.t Entry.t list)) t
  | Save : ((Status.t option -> string -> PersonCore.t Entry.t list option -> Kind.Dance.t -> (VersionCore.t Entry.t * VersionParameters.t) list option -> SetOrder.t -> DanceCore.t Entry.t list option -> Datetime.t -> Datetime.t -> 'w), 'w, SetCore.t Entry.t) t
  | Delete : ((SetCore.t Slug.t -> 'w), 'w, unit) t
  | Pdf : ((SetParameters.t option -> SetCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W Delete; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(SetCore)) @@ return (module Entry.J(SetCore))
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module SetCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(SetCore))))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "name" (module JString) @@ query_opt "conceptors" (module JList(Entry.J(PersonCore))) @@ query "kind" (module Kind.Dance) @@ query_opt "contents" (module JList(JPair(Entry.J(VersionCore))(VersionParameters))) @@ query "order" (module SetOrder) @@ query_opt "dances" (module JList(Entry.J(DanceCore))) @@ query "modified-at" (module Datetime) @@ query "created-at" (module Datetime) @@ return (module Entry.J(SetCore))
  | Delete -> literal "update" @@ variable (module SSlug(SetCore)) @@ return (module JUnit)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module SetParameters) @@ variable (module SSlug(SetCore)) @@ return (module JVoid)
