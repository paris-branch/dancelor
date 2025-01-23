open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

let _key = "version"

type t = {
  tune: Tune.t Slug.t;
  bars: int;
  key: Music.key;
  structure: string;
  sources: string list; [@default []] (* FIXME: remove from DB *)
  arrangers: Person.t Slug.t list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  content: string;
}
[@@deriving yojson, make, show {with_path = false}, fields]

let tune = tune % Entry.value
let bars = bars % Entry.value
let key = key % Entry.value
let structure = structure % Entry.value
let sources = sources % Entry.value
let arrangers = arrangers % Entry.value
let remark = remark % Entry.value
let disambiguation = disambiguation % Entry.value
let content = content % Entry.value

let equal version1 version2 = Slug.equal' (Entry.slug version1) (Entry.slug version2)

module Filter = struct
  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Tune of Tune.Filter.t
    | Key of Music.key
    | Kind of Kind.Version.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let tune' = Formula.pred % tune
  let key' = Formula.pred % key
  let kind' = Formula.pred % kind
end
