open Nes
open Madge
open Model_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Get : (unit Entry.Id.t -> 'w, 'w, Any.t) t
  | Get_rows : (Any_id.t list -> 'w, 'w, Any_row.t list) t
  | Search : ((Slice.t -> Filter.Any.t -> 'w), 'w, Any_row.t search_result) t
  | Search_context : ((Filter.Any.t -> Any_id.t -> 'w), 'w, Any_id.t search_context_result) t
  | Search_new : (Slice.t -> string -> 'w, 'w, Any_row.t search_result) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Get -> variable (module Entry.Id.S(SUnit)) @@ get (module Any)
    | Get_rows -> literal "get-rows" @@ body "ids" (module JList(Any_id)) @@ post (module JList(Any_row))
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Any) @@ get (module Utils.Search_result(Any_row))
    | Search_context -> literal "context" @@ query "filter" (module Filter.Any) @@ query "element" (module Any_id) @@ get (module Utils.Search_context_result(Any_id))
    | Search_new -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module JString) @@ get (module Utils.Search_result(Any_row))
