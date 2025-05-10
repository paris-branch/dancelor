open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
  | Search : ((Slice.t -> Filter.Any.t -> 'w), 'w, (int * Core.Any.t list)) t
  | SearchContext : ((Filter.Any.t -> Core.Any.t -> 'w), 'w, (int * Core.Any.t option * int * Core.Any.t option)) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Any) @@ get (module JPair(JInt)(JList(Core.Any)))
    | SearchContext -> literal "context" @@ query "filter" (module Filter.Any) @@ query "element" (module Core.Any) @@ get (module JQuad(JInt)(JOption(Core.Any))(JInt)(JOption(Core.Any)))
