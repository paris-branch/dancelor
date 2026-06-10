open Nes
open Madge
open Model_new
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Create : (Tune.t -> 'w, 'w, Tune_id.t) t
  | Search : (Slice.t -> (Tune.t, Filter.Tune.t) Formula_entry.public -> 'w, 'w, Tune_row.t search_result) t
  | Search_new : (Slice.t -> string -> 'w, 'w, Tune_row.t search_result) t
  | Get : (Tune_id.t -> 'w, 'w, Tune.entry) t
  | Get_row : (Tune_id.t -> 'w, 'w, Tune_row.t) t
  | Get_view : (Tune_id.t -> 'w, 'w, Tune_view.t) t
  | Update : (Tune_id.t -> Tune.t -> 'w, 'w, unit) t
  | Delete : (Tune_id.t -> 'w, 'w, unit) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Create -> body "tune" (module Tune) @@ post (module Tune_id)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Formula_entry.JPublic(Tune)(Filter.Tune)) @@ get (module Utils.Search_result(Tune_row))
    | Search_new -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module JString) @@ get (module Utils.Search_result(Tune_row))
    | Get -> variable (module Tune_id) @@ get (module Entry.JPublic(Tune))
    | Get_row -> variable (module Tune_id) @@ literal "row" @@ get (module Tune_row)
    | Get_view -> variable (module Tune_id) @@ literal "view" @@ get (module Tune_view)
    | Update -> variable (module Tune_id) @@ body "tune" (module Tune) @@ put (module JUnit)
    | Delete -> variable (module Tune_id) @@ delete (module JUnit)
