open Nes
open Madge
open Model_new
open Search_new
open Model_builder.Core

type (_, _, _) t =
  | Create : (Dance.t -> 'w, 'w, Dance_id.t) t
  | Search : (Slice.t -> Dance_query.t -> 'w, 'w, Dance_row.t Search_result.t) t
  | Get : (Dance_id.t -> 'w, 'w, Dance.entry) t
  | Get_row : (Dance_id.t -> 'w, 'w, Dance_row.t) t
  | Get_view : (Dance_id.t -> 'w, 'w, Dance_view.t) t
  | Update : (Dance_id.t -> Dance.t -> 'w, 'w, unit) t
  | Delete : (Dance_id.t -> 'w, 'w, unit) t
  | Tunes : (Dance_id.t -> 'w, 'w, Tune_row.t list) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Create -> body "dance" (module Dance) @@ post (module Dance_id)
    | Search -> literal "search" @@ query "slice" (module Slice) @@ query "query" (module Dance_query) @@ get (module Make_search_result(Dance_row))
    | Get -> variable (module Dance_id) @@ get (module Entry.JPublic(Dance))
    | Get_row -> variable (module Dance_id) @@ literal "row" @@ get (module Dance_row)
    | Get_view -> variable (module Dance_id) @@ literal "view" @@ get (module Dance_view)
    | Update -> variable (module Dance_id) @@ body "dance" (module Dance) @@ put (module JUnit)
    | Delete -> variable (module Dance_id) @@ delete (module JUnit)
    | Tunes -> variable (module Dance_id) @@ literal "tunes" @@ get (module JList(Tune_row))
