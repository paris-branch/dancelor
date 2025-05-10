open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
(* Actions without specific book *)
| Create : ((Core.Book.t -> 'w), 'w, Core.Book.t Entry.t) t
| Search : ((Slice.t -> Filter.Book.t -> 'w), 'w, (int * Core.Book.t Entry.t list)) t
(* Actions on a specific book *)
| Get : ((Core.Book.t Slug.t -> 'w), 'w, Core.Book.t Entry.t) t
| Update : ((Core.Book.t Slug.t -> Core.Book.t -> 'w), 'w, Core.Book.t Entry.t) t
(* Files related to a book *)
| Pdf : ((Core.Book.t Slug.t -> Core.BookParameters.t -> RenderingParameters.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific book *)
    | Create -> body "book" (module Core.Book) @@ post (module Entry.J(Core.Book))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Book) @@ get (module JPair(JInt)(JList(Entry.J(Core.Book))))
    (* Actions on a specific book *)
    | Get -> variable (module SSlug(Core.Book)) @@ get (module Entry.J(Core.Book))
    | Update -> variable (module SSlug(Core.Book)) @@ body "book" (module Core.Book) @@ put (module Entry.J(Core.Book))
    (* Files related to a book *)
    | Pdf -> variable (module SSlug(Core.Book)) ~suffix: ".pdf" @@ query "parameters" (module Core.BookParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ void ()
