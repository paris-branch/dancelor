open Nes

let _key = "set"

type t = {
  name: NEString.t;
  conceptors: Person.t Entry.Id.t list; [@default []]
  kind: Kind.Dance.t;
  contents: (Version.t Entry.Id.t * VersionParameters.t) list; [@key "versions-and-parameters"] [@default []]
  order: SetOrder.t;
  instructions: string; [@default ""]
  dances: Dance.t Entry.Id.t list; [@default []]
  remark: string; [@default ""]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~name ?conceptors ~kind ?contents ~order ?dances () =
  let name = NEString.map_exn (String.remove_duplicates ~char: ' ') name in
  let conceptors = Option.map (List.map Entry.id) conceptors in
  let contents = Option.map (List.map (fun (version, parameters) -> (Entry.id version, parameters))) contents in
  let dances = Option.map (List.map Entry.id) dances in
  make ~name ?conceptors ~kind ?contents ~order ?dances ()

let name' = name % Entry.value
let kind' = kind % Entry.value
let order' = order % Entry.value
let instructions' = instructions % Entry.value
let remark' = remark % Entry.value

let slug = NesSlug.of_string % NEString.to_string % name
let slug' = slug % Entry.value

let set_contents contents set =
  {set with contents = List.map (fun (version, parameters) -> (Entry.id version, parameters)) contents}

type warning =
  | Empty
  | Duplicate_tune of Tune.t Entry.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]
