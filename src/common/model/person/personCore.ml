open Nes

let _key = "person"

type t = {
  slug: t Slug.t;
  status: Status.t; [@default Status.bot]
  name: string;
  scddb_id: int option; [@default None] [@key "scddb-id"]
  modified_at: Datetime.t; [@key "modified-at"]
  created_at: Datetime.t [@key "created-at"]
}
[@@deriving yojson, make, show {with_path = false}, fields]

module Filter = struct
  let _key = "person-filter"

  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
end
