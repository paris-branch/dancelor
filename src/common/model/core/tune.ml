open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

let _key = "tune"

type t = {
  name: string;
  alternative_names: string list; [@key "alternative-names"] [@default []]
  kind: Kind.Base.t;
  composers: Person.t Slug.t list; [@default []]
  dances: Dance.t Slug.t list; [@default []]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  date: PartialDate.t option; [@default None] (** When the tune was composed. *)
}
[@@deriving yojson, make, show {with_path = false}, fields]

let name = name % Entry.value
let alternative_names = alternative_names % Entry.value
let kind = kind % Entry.value
let composers = composers % Entry.value
let dances = dances % Entry.value
let remark = remark % Entry.value
let scddb_id = scddb_id % Entry.value
let date = date % Entry.value

(* FIXME: Can't we push this into TuneLifter? *)
let compare : t Entry.t -> t Entry.t -> int = Slug.compare_slugs_or ~fallback: Stdlib.compare Entry.slug'
let equal tune1 tune2 = compare tune1 tune2 = 0

module Filter = struct
  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | ExistsComposer of Person.Filter.t (** one of the composers of the list passes the filter *)
    | Kind of Kind.Base.Filter.t
    | ExistsDance of Dance.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let existsComposer' = Formula.pred % existsComposer
  let kind' = Formula.pred % kind
  let existsDance' = Formula.pred % existsDance
end
