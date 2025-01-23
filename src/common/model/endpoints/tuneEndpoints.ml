open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  (* Actions without specific tune *)
  | Create : ((TuneCore.t -> 'w), 'w, TuneCore.t Entry.t) t
  | Search : ((Slice.t -> TuneCore.Filter.t -> 'w), 'w, (int * TuneCore.t Entry.t list)) t
  (* Actions on a specific tune *)
  | Get : ((TuneCore.t Slug.t -> 'w), 'w, TuneCore.t Entry.t) t
  | Update : ((TuneCore.t Slug.t -> TuneCore.t -> 'w), 'w, TuneCore.t Entry.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific tune *)
  | Create -> query "tune" (module TuneCore) @@ post (module Entry.J(TuneCore))
  | Search -> query "slice" (module Slice) @@ query "filter" (module TuneCore.Filter) @@ get (module JPair(JInt)(JList(Entry.J(TuneCore))))
  (* Actions on a specific tune *)
  | Get -> variable (module SSlug(TuneCore)) @@ get (module Entry.J(TuneCore))
  | Update -> variable (module SSlug(TuneCore)) @@ query "tune" (module TuneCore) @@ put (module Entry.J(TuneCore))
