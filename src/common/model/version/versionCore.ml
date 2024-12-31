open Nes

let _key = "version"

type t = {
  slug: t Slug.t;
  status: Status.t; [@default Status.bot]
  tune: TuneCore.t Slug.t;
  bars: int;
  key: Music.key;
  structure: string;
  sources: string list; [@default []] (* FIXME: remove from DB *)
  arrangers: PersonCore.t Slug.t list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  broken: bool; [@default false]
  modified_at: Datetime.t; [@key "modified-at"]
  created_at: Datetime.t [@key "created-at"]
}
[@@deriving make, show {with_path = false}, yojson, fields]

let equal version1 version2 = Slug.equal' (slug version1) (slug version2)

module Filter = struct
  let _key = "version-filter"

  type predicate =
    | Is of t Slug.t
    | Tune of TuneCore.Filter.t
    | Key of Music.key
    | Kind of Kind.Version.Filter.t
    | Broken
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let tune' = Formula.pred % tune
  let key' = Formula.pred % key
  let kind' = Formula.pred % kind
  let broken' = Formula.pred broken
end
