open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
  | Search : ((Slice.t -> Any.Filter.t -> 'w), 'w, (int * Any.t list)) t
  | SearchContext : ((Any.Filter.t -> Any.t -> 'w), 'w, (int * Any.t option * int * Any.t option)) t
[@@deriving madge_wrapped_endpoints]

let to_string : type a w r. (a, w, r) t -> string = function
  | Search -> "Search"
  | SearchContext -> "SearchContext"

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Search -> query "slice" (module Slice) @@ query "filter" (module Any.Filter) @@ get (module JPair(JInt)(JList(Any)))
    | SearchContext -> literal "context" @@ query "filter" (module Any.Filter) @@ query "element" (module Any) @@ get (module JQuad(JInt)(JOption(Any))(JInt)(JOption(Any)))
