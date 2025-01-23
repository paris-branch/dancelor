open Nes
open Madge
open Dancelor_common_database
open Dancelor_common_model_utils
open Dancelor_common_model_core

type (_, _, _) t =
  (* Actions without a specific dance *)
  | Create : ((Dance.t -> 'w), 'w, Dance.t Entry.t) t
  | Search : ((Slice.t -> Dance.Filter.t -> 'w), 'w, (int * Dance.t Entry.t list)) t
  (* Actions on a specific dance *)
  | Get : ((Dance.t Slug.t -> 'w), 'w, Dance.t Entry.t) t
  | Update : ((Dance.t Slug.t -> Dance.t -> 'w), 'w, Dance.t Entry.t) t
  (* Files related to a dance *)
  | Pdf : ((SetParameters.t -> Dance.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without a specific dance *)
  | Create -> query "dance" (module Dance) @@ post (module Entry.J(Dance))
  | Search -> query "slice" (module Slice) @@ query "filter" (module Dance.Filter) @@ get (module JPair(JInt)(JList(Entry.J(Dance))))
  (* Actions on a specific dance *)
  | Get -> variable (module SSlug(Dance)) @@ get (module Entry.J(Dance))
  | Update -> variable (module SSlug(Dance)) @@ query "dance" (module Dance) @@ put (module Entry.J(Dance))
  (* Files related to a dance *)
  | Pdf -> query "parameters" (module SetParameters) @@ variable (module SSlug(Dance)) ~suffix: ".pdf" @@ void ()
