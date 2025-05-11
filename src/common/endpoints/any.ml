open Nes
open Madge
open ModelBuilder.Core
module Filter = FilterBuilder.Core

type (_, _, _) t =
  | Search : ((Slice.t -> Filter.Any.t -> 'w), 'w, (int * Any.t list)) t
  | SearchContext : ((Filter.Any.t -> Any.t -> 'w), 'w, (int * Any.t option * int * Any.t option)) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Any) @@ get (module JPair(JInt)(JList(Any)))
    | SearchContext -> literal "context" @@ query "filter" (module Filter.Any) @@ query "element" (module Any) @@ get (module JQuad(JInt)(JOption(Any))(JInt)(JOption(Any)))
