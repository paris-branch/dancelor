open Nes
open Madge
open Model_new
open Search_new
open Model_builder.Core

type (_, _, _) t =
  | Create : (Book.t -> Entry.Access.Private.t -> 'w, 'w, Book_id.t) t
  | Search : (Slice.t -> Book_query.t -> 'w, 'w, Book_row.t Search_result.t) t
  | Get : (Book_id.t -> 'w, 'w, Book.entry) t
  | Get_row : (Book_id.t -> 'w, 'w, Book_row.t) t
  | Get_view : (Book_id.t -> 'w, 'w, Book_view.t) t
  | Get_rows : (Book_id.t list -> 'w, 'w, Book_row.t list) t
  | Update : (Book_id.t -> Book.t -> Entry.Access.Private.t -> 'w, 'w, unit) t
  | Delete : (Book_id.t -> 'w, 'w, unit) t
  | Build_pdf : (Book_id.t -> Book_parameters.t -> Rendering_parameters.t -> 'w, 'w, Job_id.t Job.registration_response) t
  | Build_zip : (Book_id.t -> Book_parameters.t -> Rendering_parameters.t -> 'w, 'w, Job_id.t Job.registration_response) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Create -> body "book" (module Book) @@ body "access" (module Entry.Access.Private) @@ post (module Book_id)
    | Search -> literal "search" @@ query_json "slice" (module Slice) @@ query_json "query" (module Book_query) @@ get (module Make_search_result(Book_row))
    | Get -> variable (module Book_id) @@ get (module Entry.JPrivate(Book))
    | Get_row -> variable (module Book_id) @@ literal "row" @@ get (module Book_row)
    | Get_view -> variable (module Book_id) @@ literal "view" @@ get (module Book_view)
    | Get_rows -> literal "rows" @@ body "ids" (module JList(Book_id)) @@ post (module JList(Book_row))
    | Update -> variable (module Book_id) @@ body "book" (module Book) @@ body "access" (module Entry.Access.Private) @@ put (module JUnit)
    | Delete -> variable (module Book_id) @@ delete (module JUnit)
    | Build_pdf -> literal "build-pdf" @@ variable (module Book_id) @@ query_json_def "parameters" (module Book_parameters) ~eq: Book_parameters.equal ~def: Book_parameters.none @@ query_json_def "rendering-parameters" (module Rendering_parameters) ~eq: Rendering_parameters.equal ~def: Rendering_parameters.none @@ post (module Job.Registration_response(Job_id))
    | Build_zip -> literal "build-zip" @@ variable (module Book_id) @@ query_json_def "parameters" (module Book_parameters) ~eq: Book_parameters.equal ~def: Book_parameters.none @@ query_json_def "rendering-parameters" (module Rendering_parameters) ~eq: Rendering_parameters.equal ~def: Rendering_parameters.none @@ post (module Job.Registration_response(Job_id))
