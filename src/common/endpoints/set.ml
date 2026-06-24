open Nes
open Madge
open Model_new
open Search_new
open Model_builder.Core

type (_, _, _) t =
  | Create : (Set.t -> Entry.Access.Private.t -> 'w, 'w, Set_id.t) t
  | Search : (Slice.t -> Set_query.t -> 'w, 'w, Set_row.t Search_result.t) t
  | Get : (Set_id.t -> 'w, 'w, Set.entry) t
  | Get_row : (Set_id.t -> 'w, 'w, Set_row.t) t
  | Get_view : (Set_id.t -> 'w, 'w, Set_view.t) t
  | Get_rows : (Set_id.t list -> 'w, 'w, Set_row.t list) t
  | Update : (Set_id.t -> Set.t -> Entry.Access.Private.t -> 'w, 'w, unit) t
  | Delete : (Set_id.t -> 'w, 'w, unit) t
  | Build_pdf : (Set_id.t -> Set_parameters.t -> Rendering_parameters.t -> 'w, 'w, Job_id.t Job.registration_response) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Create -> body "set" (module Set) @@ body "access" (module Entry.Access.Private) @@ post (module Set_id)
    | Search -> literal "search" @@ query "slice" (module Slice) @@ query "query" (module Set_query) @@ get (module Make_search_result(Set_row))
    | Get -> variable (module Set_id) @@ get (module Entry.JPrivate(Set))
    | Get_row -> variable (module Set_id) @@ literal "row" @@ get (module Set_row)
    | Get_view -> variable (module Set_id) @@ literal "view" @@ get (module Set_view)
    | Get_rows -> literal "rows" @@ body "ids" (module JList(Set_id)) @@ post (module JList(Set_row))
    | Update -> variable (module Set_id) @@ body "set" (module Set) @@ body "access" (module Entry.Access.Private) @@ put (module JUnit)
    | Delete -> variable (module Set_id) @@ delete (module JUnit)
    | Build_pdf -> literal "build-pdf" @@ variable (module Set_id) @@ query_def "parameters" (module Set_parameters) ~eq: Set_parameters.equal Set_parameters.none @@ query_def "rendering-parameters" (module Rendering_parameters) ~eq: Rendering_parameters.equal Rendering_parameters.none @@ post (module Job.Registration_response(Job_id))
