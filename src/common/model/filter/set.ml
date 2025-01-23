open Nes
open Dancelor_common_model_utils
module Core = Dancelor_common_model_core

(* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
   of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
module SetCore = struct include Core.Set let equal _ _ = assert false end

type predicate =
  | Is of SetCore.t Slug.t
  | Name of string
  | NameMatches of string
  | ExistsConceptor of Person.t (** conceptor is defined and passes the filter *)
  | ExistsVersion of Version.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let existsConceptor' = Formula.pred % existsConceptor
let existsVersion' = Formula.pred % existsVersion
let kind' = Formula.pred % kind
