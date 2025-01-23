open Nes
open Dancelor_common_model_utils
module Core = Dancelor_common_model_core

(* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
   of [Person.t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
module PersonCore = struct include Core.Person let equal _ _ = assert false end

type predicate =
  | Is of PersonCore.t Slug.t
  | Name of string
  | NameMatches of string
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
