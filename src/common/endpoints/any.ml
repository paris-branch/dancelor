open Nes
open Madge
open Model_builder.Core
module Filter = Filter_builder.Core

type (_, _, _) t =
  | Get : (unit Entry.Id.t -> 'w, 'w, Any.t) t
  | Search : ((Slice.t -> Filter.Any.t -> 'w), 'w, (int * Any.t list)) t
  | Search_context : ((Filter.Any.t -> Any.t -> 'w), 'w, (int * Any.t option * int * Any.t option)) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Get -> variable (module Entry.Id.S(SUnit)) @@ get (module Any)
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Any) @@ get (module JPair(JInt)(JList(Any)))
    | Search_context -> literal "context" @@ query "filter" (module Filter.Any) @@ query "element" (module Any) @@ get (module JQuad(JInt)(JOption(Any))(JInt)(JOption(Any)))
