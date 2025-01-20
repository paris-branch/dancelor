open Nes
open Dancelor_common_database

let _key = "set"

type t = {
  name: string;
  conceptors: PersonCore.t Slug.t list; [@default []]
  kind: Kind.Dance.t;
  contents: (VersionCore.t Slug.t * VersionParameters.t) list; [@key "versions-and-parameters"] [@default []]
  order: SetOrder.t;
  instructions: string; [@default ""]
  dances: DanceCore.t Slug.t list; [@default []]
  remark: string; [@default ""]
}
[@@deriving yojson, make, show {with_path = false}, fields]

let name = name % Entry.value
let conceptors = conceptors % Entry.value
let kind = kind % Entry.value
let contents = contents % Entry.value
let order = order % Entry.value
let instructions = instructions % Entry.value
let dances = dances % Entry.value
let remark = remark % Entry.value

type warning =
  | Empty
  | WrongKind
  | WrongVersionBars of VersionCore.t Entry.t
  | WrongVersionKind of TuneCore.t Entry.t
  | DuplicateVersion of TuneCore.t Entry.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

module Filter = struct
  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | ExistsConceptor of PersonCore.Filter.t (** conceptor is defined and passes the filter *)
    | ExistsVersion of VersionCore.Filter.t
    | Kind of Kind.Dance.Filter.t
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let existsConceptor' = Formula.pred % existsConceptor
  let existsVersion' = Formula.pred % existsVersion
  let kind' = Formula.pred % kind
end
