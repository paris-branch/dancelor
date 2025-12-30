open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
(* Actions without specific book *)
| Create : ((Book.t -> 'w), 'w, Book.entry) t
| Search : ((Slice.t -> Filter.Book.t -> 'w), 'w, (int * Book.entry list)) t
(* Actions on a specific book *)
| Get : ((Book.t Entry.Id.t -> 'w), 'w, Book.entry) t
| Update : ((Book.t Entry.Id.t -> Book.t -> 'w), 'w, Book.entry) t
| Delete : ((Book.t Entry.Id.t -> 'w), 'w, unit) t
(* Files related to a book *)
| BuildPdf : ((Book.t Entry.Id.t -> BookParameters.t -> RenderingParameters.t -> 'w), 'w, JobId.t Job.registration_response) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without specific book *)
    | Create -> body "book" (module Book) @@ post (module Entry.JPrivate(Book))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Book) @@ get (module JPair(JInt)(JList(Entry.JPrivate(Book))))
    (* Actions on a specific book *)
    | Get -> variable (module Entry.Id.S(Book)) @@ get (module Entry.JPrivate(Book))
    | Update -> variable (module Entry.Id.S(Book)) @@ body "book" (module Book) @@ put (module Entry.JPrivate(Book))
    | Delete -> variable (module Entry.Id.S(Book)) @@ delete (module JUnit)
    (* Files related to a set *)
    | BuildPdf -> literal "build-pdf" @@ variable (module Entry.Id.S(Book)) @@ query "parameters" (module BookParameters) @@ query "rendering-parameters" (module RenderingParameters) @@ post (module Job.Registration_response(JobId))
