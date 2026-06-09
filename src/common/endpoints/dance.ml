open Nes
open Madge
open Model_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Create : (Dance.t -> 'w, 'w, Dance_id.t) t
  | Search : (Slice.t -> (Dance.t, Filter.Dance.t) Formula_entry.public -> 'w, 'w, Dance_row.t search_result) t
  | Search_new : (Slice.t -> NEString.t option -> 'w, 'w, Dance_row.t search_result) t
  | Get : (Dance_id.t -> 'w, 'w, Dance.entry) t
  | Get_row : (Dance_id.t -> 'w, 'w, Dance_row.t) t
  | Get_view : (Dance_id.t -> 'w, 'w, Dance_view.t) t
  | Update : (Dance_id.t -> Dance.t -> 'w, 'w, unit) t
  | Delete : (Dance_id.t -> 'w, 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    (* Actions without a specific dance *)
    | Create -> body "dance" (module Dance) @@ post (module Dance_id)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPublic(Dance)(Filter.Dance)) @@ get (module Utils.Search_result(Dance_row))
    | Search_new -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module JOption(JNEString)) @@ get (module Utils.Search_result(Dance_row))
    (* Actions on a specific dance *)
    | Get -> variable (module Dance_id) @@ get (module Entry.JPublic(Dance))
    | Get_row -> variable (module Dance_id) @@ literal "row" @@ get (module Dance_row)
    | Get_view -> variable (module Dance_id) @@ literal "view" @@ get (module Dance_view)
    | Update -> variable (module Dance_id) @@ body "dance" (module Dance) @@ put (module JUnit)
    | Delete -> variable (module Dance_id) @@ delete (module JUnit)
