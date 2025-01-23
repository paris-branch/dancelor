open Nes
open Dancelor_common_model_utils
module Core = Dancelor_common_model_core

(* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
   of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
module DanceCore = struct include Core.Dance let equal _ _ = assert false end

type predicate =
  | Is of DanceCore.t Slug.t
  | Name of string
  | NameMatches of string
  | Kind of Kind.Dance.Filter.t
  | ExistsDeviser of Person.t (** deviser is defined and passes the filter *)
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let kind' = Formula.pred % kind
let existsDeviser' = Formula.pred % existsDeviser
