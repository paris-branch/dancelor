open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
(* Actions without specific book *)
| Create : ((Book.t -> 'w), 'w, Book.t Entry.t) t
| Search : ((Slice.t -> Filter.Book.t -> 'w), 'w, (int * Book.t Entry.t list)) t
(* Actions on a specific book *)
| Get : ((Book.t Entry.Id.t -> 'w), 'w, Book.t Entry.t) t
| Update : ((Book.t Entry.Id.t -> Book.t -> 'w), 'w, Book.t Entry.t) t
| Delete : ((Book.t Entry.Id.t -> 'w), 'w, unit) t
(* Files related to a book *)
| BuildPdf : ((Book.t Entry.Id.t -> BookParameters.t -> RenderingParameters.t -> 'w), 'w, Job.Registration.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific book *)
    | Create -> body "book" (module Book) @@ post (module Entry.J(Book))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Book) @@ get (module JPair(JInt)(JList(Entry.J(Book))))
    (* Actions on a specific book *)
    | Get -> variable (module Entry.Id.S(Book)) @@ get (module Entry.J(Book))
    | Update -> variable (module Entry.Id.S(Book)) @@ body "book" (module Book) @@ put (module Entry.J(Book))
    | Delete -> variable (module Entry.Id.S(Book)) @@ delete (module JUnit)
    (* Files related to a set *)
    | BuildPdf -> literal "build-pdf" @@ variable (module Entry.Id.S(Book)) @@ query "parameters" (module BookParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Job.Registration)
