open Nes
open Database

let _key = "set"

type t = {
  name: string;
  conceptors: Person.t Slug.t list; [@default []]
  kind: Kind.Dance.t;
  contents: (Version.t Slug.t * VersionParameters.t) list; [@key "versions-and-parameters"] [@default []]
  order: SetOrder.t;
  instructions: string; [@default ""]
  dances: Dance.t Slug.t list; [@default []]
  remark: string; [@default ""]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

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
  | WrongVersionBars of Version.t Entry.t
  | WrongVersionKind of Tune.t Entry.t
  | DuplicateVersion of Tune.t Entry.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]
