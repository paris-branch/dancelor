open Nes
open Madge
open Dancelor_common_database

type (_, _, _) t =
  (* Actions without specific book *)
  | Create : ((BookCore.t -> 'w), 'w, BookCore.t Entry.t) t
  | Search : ((Slice.t -> BookCore.Filter.t -> 'w), 'w, (int * BookCore.t Entry.t list)) t
  (* Actions on a specific book *)
  | Get : ((BookCore.t Slug.t -> 'w), 'w, BookCore.t Entry.t) t
  | Update : ((BookCore.t Slug.t -> BookCore.t -> 'w), 'w, BookCore.t Entry.t) t
  (* Files related to a book *)
  | Pdf : ((BookParameters.t -> BookCore.t Slug.t -> 'w), 'w, Void.t) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific book *)
  | Create -> query "book" (module BookCore) @@ post (module Entry.J(BookCore))
  | Search -> query "slice" (module Slice) @@ query "filter" (module BookCore.Filter) @@ get (module JPair(JInt)(JList(Entry.J(BookCore))))
  (* Actions on a specific book *)
  | Get -> variable (module SSlug(BookCore)) @@ get (module Entry.J(BookCore))
  | Update -> variable (module SSlug(BookCore)) @@ query "book" (module BookCore) @@ put (module Entry.J(BookCore))
  (* Files related to a book *)
  | Pdf -> query "parameters" (module BookParameters) @@ variable (module SSlug(BookCore)) ~suffix: ".pdf" @@ void ()
