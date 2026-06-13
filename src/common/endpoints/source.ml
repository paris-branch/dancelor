open Nes
open Madge
open Model_new
open Search_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Create : (Source.t -> 'w, 'w, Source_id.t) t
  | Search : (Slice.t -> (Source.t, Filter.Source.t) Formula_entry.public -> 'w, 'w, Source_row.t Search_result.t) t
  | Search_new : (Slice.t -> Query_string.t -> 'w, 'w, Source_row.t Search_result.t) t
  | Get : (Source_id.t -> 'w, 'w, Source.entry) t
  | Get_row : (Source_id.t -> 'w, 'w, Source_row.t) t
  | Get_view : (Source_id.t -> 'w, 'w, Source_view.t) t
  | Update : (Source_id.t -> Source.t -> 'w, 'w, unit) t
  | Delete : (Source_id.t -> 'w, 'w, unit) t
  | Cover : (Source_id.t -> 'w, 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Create -> body "source" (module Source) @@ post (module Source_id)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPublic(Source)(Filter.Source)) @@ get (module Make_search_result(Source_row))
    | Search_new -> literal "search" @@ query "slice" (module Slice) @@ query "query" (module Query_string) @@ get (module Make_search_result(Source_row))
    | Get -> variable (module Source_id) @@ get (module Entry.JPublic(Source))
    | Get_row -> variable (module Source_id) @@ literal "row" @@ get (module Source_row)
    | Get_view -> variable (module Source_id) @@ literal "view" @@ get (module Source_view)
    | Update -> variable (module Source_id) @@ body "source" (module Source) @@ put (module JUnit)
    | Delete -> variable (module Source_id) @@ delete (module JUnit)
    | Cover -> variable (module Source_id) @@ literal "cover.webp" @@ void ()
