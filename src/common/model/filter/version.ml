open Nes
open Dancelor_common_model_utils
module Core = Dancelor_common_model_core

(* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
   of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
module VersionCore = struct include Core.Version let equal _ _ = assert false end

type predicate =
  | Is of VersionCore.t Slug.t
  | Tune of Tune.t
  | Key of Music.key
  | Kind of Kind.Version.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let tune' = Formula.pred % tune
let key' = Formula.pred % key
let kind' = Formula.pred % kind
