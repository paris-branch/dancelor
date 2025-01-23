open Nes
open Dancelor_common_model_utils
module Core = Dancelor_common_model_core

(* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
   of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
module TuneCore = struct include Core.Tune let equal _ _ = assert false end

type predicate =
  | Is of TuneCore.t Slug.t
  | Name of string
  | NameMatches of string
  | ExistsComposer of Person.t (** one of the composers of the list passes the filter *)
  | Kind of Kind.Base.Filter.t
  | ExistsDance of Dance.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let existsComposer' = Formula.pred % existsComposer
let kind' = Formula.pred % kind
let existsDance' = Formula.pred % existsDance
