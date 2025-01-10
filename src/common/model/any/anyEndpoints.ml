open Nes
open Madge
open Madge_common

type (_, _, _) t =
  | Search : ((Slice.t option -> float option -> AnyCore.Filter.t -> 'w), 'w, (int * AnyCore.t list)) t
  | SearchContext : ((float option -> AnyCore.Filter.t -> AnyCore.t -> 'w), 'w, (int * AnyCore.t option * int * AnyCore.t option)) t

(* FIXME: make a simple PPX for the following *)
type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Search; W SearchContext]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Search -> literal "search" @@ query_opt "slice" (module Slice) @@ query_opt "threshold" (module MFloat) @@ query "filter" (module AnyCore.Filter) @@ return (module MPair(MInteger)(MList(AnyCore)))
  | SearchContext -> literal "search-context" @@ query_opt "threshold" (module MFloat) @@ query "filter" (module AnyCore.Filter) @@ query "element" (module AnyCore) @@ return (module MQuadruple(MInteger)(MOption(AnyCore))(MInteger)(MOption(AnyCore)))
