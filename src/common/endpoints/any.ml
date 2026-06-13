open Nes
open Madge
open Model_new
open Search_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Get : (unit Entry.Id.t -> 'w, 'w, Any.t) t
  | Get_rows : (Any_id.t list -> 'w, 'w, Any_row.t list) t
  | Newest : (int -> 'w, 'w, Any_row.t list) t
  | Search_context : (Filter.Any.t -> Any_id.t -> 'w, 'w, Any_id.t Search_context_result.t) t
  | Search_new : (Slice.t -> Any_query.t -> 'w, 'w, Any_row.t Search_result.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Get -> variable (module Entry.Id.S(SUnit)) @@ get (module Any)
    | Get_rows -> literal "get-rows" @@ body "ids" (module JList(Any_id)) @@ post (module JList(Any_row))
    | Newest -> literal "newest" @@ query "limit" (module JInt) @@ get (module JList(Any_row))
    | Search_context -> literal "context" @@ query "filter" (module Filter.Any) @@ query "element" (module Any_id) @@ get (module Make_search_context_result(Any_id))
    | Search_new -> literal "search" @@ query "slice" (module Slice) @@ query "query" (module Any_query) @@ get (module Make_search_result(Any_row))
