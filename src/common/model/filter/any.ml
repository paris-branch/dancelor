open Nes
open Dancelor_common_model_utils

(* NOTE: This [Raw] variant is a bit artificial, when we could already be
   inheriting the various [raw] cases, of the other filters. However, this
   would unfold text formulas into a big disjunction at the syntactic level,
   and we would rather avoid that. *)
type predicate =
  | Raw of string
  | Type of Dancelor_common_model_core.Any.Type.t
  (* lifting predicates: *)
  | Person of Person.t
  | Dance of Dance.t
  | Book of Book.t
  | Set of Set.t
  | Tune of Tune.t
  | Version of Version.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let raw' = Formula.pred % raw
let type_' = Formula.pred % type_
let person' = Formula.pred % person
let dance' = Formula.pred % dance
let book' = Formula.pred % book
let set' = Formula.pred % set
let tune' = Formula.pred % tune
let version' = Formula.pred % version
