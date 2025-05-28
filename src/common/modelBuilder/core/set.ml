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

let make ~name ?conceptors ~kind ?contents ~order ?dances () =
  let name = String.remove_duplicates ~char: ' ' name in
  let conceptors = Option.map (List.map Entry.slug) conceptors in
  let contents = Option.map (List.map (fun (version, parameters) -> (Entry.slug version, parameters))) contents in
  let dances = Option.map (List.map Entry.slug) dances in
  make ~name ?conceptors ~kind ?contents ~order ?dances ()

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
