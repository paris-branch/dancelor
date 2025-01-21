open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((TuneCore.t Slug.t -> 'w), 'w, TuneCore.t Entry.t) t
  | Search : ((Slice.t option -> float option -> TuneCore.Filter.t -> 'w), 'w, (int * TuneCore.t Entry.t list)) t
  | Save : ((TuneCore.t -> 'w), 'w, TuneCore.t Entry.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Save]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(TuneCore)) @@ return (module Entry.J(TuneCore))
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module JFloat) @@ query "filter" (module TuneCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(TuneCore))))
  | Save -> literal "save" @@ query "tune" (module TuneCore) @@ return (module Entry.J(TuneCore))
