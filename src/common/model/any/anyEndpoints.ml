open Nes
open Madge

type (_, _, _) t =
  | Search : ((Slice.t -> AnyCore.Filter.t -> 'w), 'w, (int * AnyCore.t list)) t
  | SearchContext : ((AnyCore.Filter.t -> AnyCore.t -> 'w), 'w, (int * AnyCore.t option * int * AnyCore.t option)) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Search; W SearchContext]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Search -> literal "search" @@ query "slice" (module Slice) @@ query "filter" (module AnyCore.Filter) @@ return (module JPair(JInt)(JList(AnyCore)))
  | SearchContext -> literal "search-context" @@ query "filter" (module AnyCore.Filter) @@ query "element" (module AnyCore) @@ return (module JQuad(JInt)(JOption(AnyCore))(JInt)(JOption(AnyCore)))
