open Nes
open Dancelor_common_database

let _key = "dance"

type core = {
  name: string;
  kind: Kind.Dance.t;
  devisers: PersonCore.t Slug.t list; [@default []]
  two_chords: bool option; [@default None] [@key "two-chords"]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  disambiguation: string; [@default ""]
  date: PartialDate.t option; [@default None] (** When the dance was devised. *)
}
[@@deriving make, show {with_path = false}, yojson, fields]

type t = core Entry.t
[@@deriving yojson, show]

let name = name % Entry.value
let kind = kind % Entry.value
let devisers = devisers % Entry.value
let two_chords = two_chords % Entry.value
let scddb_id = scddb_id % Entry.value
let disambiguation = disambiguation % Entry.value
let date = date % Entry.value

module Filter = struct
  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | Kind of Kind.Dance.Filter.t
    | ExistsDeviser of PersonCore.Filter.t (** deviser is defined and passes the filter *)
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let kind' = Formula.pred % kind
  let existsDeviser' = Formula.pred % existsDeviser
end
