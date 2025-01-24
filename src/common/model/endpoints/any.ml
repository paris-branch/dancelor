open Nes
open Madge

open Core

type (_, _, _) t =
  | Search : ((Slice.t -> Filter.Any.t -> 'w), 'w, (int * Any.t list)) t
  | SearchContext : ((Filter.Any.t -> Any.t -> 'w), 'w, (int * Any.t option * int * Any.t option)) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Search; W SearchContext]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Search -> query "slice" (module Slice) @@ query "filter" (module Filter.Any) @@ get (module JPair(JInt)(JList(Any)))
  | SearchContext -> literal "context" @@ query "filter" (module Filter.Any) @@ query "element" (module Any) @@ get (module JQuad(JInt)(JOption(Any))(JInt)(JOption(Any)))
