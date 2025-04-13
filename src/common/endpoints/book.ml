open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific book *)
| Create : ((Book.t -> 'w), 'w, Book.t Entry.t) t
| Search : ((Slice.t -> Book.Filter.t -> 'w), 'w, (int * Book.t Entry.t list)) t
(* Actions on a specific book *)
| Get : ((Book.t Slug.t -> 'w), 'w, Book.t Entry.t) t
| Update : ((Book.t Slug.t -> Book.t -> 'w), 'w, Book.t Entry.t) t
(* Files related to a book *)
| Pdf : ((BookParameters.t -> Book.t Slug.t -> 'w), 'w, Void.t) t

let to_string : type a w r. (a, w, r) t -> string = function
  | Create -> "Create"
  | Search -> "Search"
  | Get -> "Get"
  | Update -> "Update"
  | Pdf -> "Pdf"

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get; W Search; W Create; W Update; W Pdf]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  (* Actions without specific book *)
  | Create -> query "book" (module Book) @@ post (module Entry.J(Book))
  | Search -> query "slice" (module Slice) @@ query "filter" (module Book.Filter) @@ get (module JPair(JInt)(JList(Entry.J(Book))))
  (* Actions on a specific book *)
  | Get -> variable (module SSlug(Book)) @@ get (module Entry.J(Book))
  | Update -> variable (module SSlug(Book)) @@ query "book" (module Book) @@ put (module Entry.J(Book))
  (* Files related to a book *)
  | Pdf -> query "parameters" (module BookParameters) @@ variable (module SSlug(Book)) ~suffix: ".pdf" @@ void ()
