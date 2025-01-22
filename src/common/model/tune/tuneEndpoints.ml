open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  | Get : ((TuneCore.t Slug.t -> 'w), 'w, TuneCore.t Entry.t) t
  | Search : ((Slice.t -> TuneCore.Filter.t -> 'w), 'w, (int * TuneCore.t Entry.t list)) t
  | Create : ((TuneCore.t -> 'w), 'w, TuneCore.t Entry.t) t
  | Update : ((TuneCore.t Slug.t -> TuneCore.t -> 'w), 'w, TuneCore.t Entry.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> literal "get" @@ variable (module SSlug(TuneCore)) @@ return (module Entry.J(TuneCore))
  | Search -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module TuneCore.Filter) @@ return (module JPair(JInt)(JList(Entry.J(TuneCore))))
  | Create -> literal "create" @@ query "tune" (module TuneCore) @@ return (module Entry.J(TuneCore))
  | Update -> literal "update" @@ variable (module SSlug(TuneCore)) @@ query "tune" (module TuneCore) @@ return (module Entry.J(TuneCore))
