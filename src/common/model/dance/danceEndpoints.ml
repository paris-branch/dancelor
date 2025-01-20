open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((DanceCore.t Slug.t -> 'w), 'w, DanceCore.t) t
  | Search : ((Slice.t option -> float option -> DanceCore.Filter.t -> 'w), 'w, (int * DanceCore.t list)) t
  | Save : ((Status.t option -> string -> Kind.Dance.t -> PersonCore.t list option -> bool option -> int option -> string option -> PartialDate.t option -> Datetime.t -> Datetime.t -> 'w), 'w, DanceCore.t) t
  | Pdf : ((SetParameters.t option -> DanceCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(DanceCore)) @@ return (module DanceCore)
  | Pdf -> literal "pdf" @@ query_opt "parameters" (module SetParameters) @@ variable (module SSlug(DanceCore)) @@ return (module JVoid)
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module DanceCore.Filter) @@ return (module JPair(JInt)(JList(DanceCore)))
  | Save -> literal "make-and-save" @@ query_opt "status" (module Status) @@ query "name" (module JString) @@ query "kind" (module Kind.Dance) @@ query_opt "devisers" (module JList(PersonCore)) @@ query_opt "two_chords" (module JBool) @@ query_opt "scddb_id" (module JInt) @@ query_opt "disambiguation" (module JString) @@ query_opt "date" (module PartialDate) @@ query "modified_at" (module Datetime) @@ query "created_at" (module Datetime) @@ return (module DanceCore)
