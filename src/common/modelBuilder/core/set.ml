open Ppx_yojson_conv_lib.Yojson_conv
open Nes

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

let name' = name % Entry.value
let kind' = kind % Entry.value
let order' = order % Entry.value
let instructions' = instructions % Entry.value
let remark' = remark % Entry.value

type warning =
  | Empty
  | WrongKind
  | WrongVersionBars of Version.t Entry.t
  | WrongVersionKind of Tune.t Entry.t
  | DuplicateVersion of Tune.t Entry.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]
