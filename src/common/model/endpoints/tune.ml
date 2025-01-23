open Nes
open Madge
open Dancelor_common_database
open Dancelor_common_model_utils
open Dancelor_common_model_core
module Filter = Dancelor_common_model_filter

type (_, _, _) t =
  (* Actions without specific tune *)
  | Create : ((Tune.t -> 'w), 'w, Tune.t Entry.t) t
  | Search : ((Slice.t -> Filter.Tune.t -> 'w), 'w, (int * Tune.t Entry.t list)) t
  (* Actions on a specific tune *)
  | Get : ((Tune.t Slug.t -> 'w), 'w, Tune.t Entry.t) t
  | Update : ((Tune.t Slug.t -> Tune.t -> 'w), 'w, Tune.t Entry.t) t

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific tune *)
  | Create -> query "tune" (module Tune) @@ post (module Entry.J(Tune))
  | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Tune) @@ get (module JPair(JInt)(JList(Entry.J(Tune))))
  (* Actions on a specific tune *)
  | Get -> variable (module SSlug(Tune)) @@ get (module Entry.J(Tune))
  | Update -> variable (module SSlug(Tune)) @@ query "tune" (module Tune) @@ put (module Entry.J(Tune))
