open Nes
open Madge
open Model_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Create : (Book.t -> Entry.Access.Private.t -> 'w, 'w, Book_id.t) t
  | Search : (Slice.t -> (Book.t, Filter.Book.t) Formula_entry.private_ -> 'w, 'w, Book_row.t search_result) t
  | Get : (Book_id.t -> 'w, 'w, Book.entry) t
  | Get_row : (Book_id.t -> 'w, 'w, Book_row.t) t
  | Get_view : (Book_id.t -> 'w, 'w, Book_view.t) t
  | Update : (Book_id.t -> Book.t -> Entry.Access.Private.t -> 'w, 'w, unit) t
  | Delete : (Book_id.t -> 'w, 'w, unit) t
  | Build_pdf : (Book_id.t -> Book_parameters.t -> Rendering_parameters.t -> 'w, 'w, Job_id.t Job.registration_response) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Create -> body "book" (module Book) @@ body "access" (module Entry.Access.Private) @@ post (module Book_id)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPrivate(Book)(Filter.Book)) @@ get (module Utils.Search_result(Book_row))
    | Get -> variable (module Book_id) @@ get (module Entry.JPrivate(Book))
    | Get_row -> variable (module Book_id) @@ literal "row" @@ get (module Book_row)
    | Get_view -> variable (module Book_id) @@ literal "view" @@ get (module Book_view)
    | Update -> variable (module Book_id) @@ body "book" (module Book) @@ body "access" (module Entry.Access.Private) @@ put (module JUnit)
    | Delete -> variable (module Book_id) @@ delete (module JUnit)
    | Build_pdf -> literal "build-pdf" @@ variable (module Book_id) @@ query "parameters" (module Book_parameters) @@ query "rendering-parameters" (module Rendering_parameters) @@ post (module Job.Registration_response(Job_id))
