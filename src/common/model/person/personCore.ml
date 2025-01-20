open Nes
open Dancelor_common_database

let _key = "person"

type t = {
  name: string;
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving yojson, make, show {with_path = false}, fields]

let name = name % Entry.value
let scddb_id = scddb_id % Entry.value

module Filter = struct
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
