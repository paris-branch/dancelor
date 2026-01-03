open Nes

type t = {
  created_at: Datetime.t; [@key "created-at"]
  modified_at: Datetime.t; [@key "modified-at"]
}
[@@deriving yojson, show {with_path = false}, fields]

let make ?created_at ?modified_at () =
  let now = Datetime.now () in
  {
    created_at = Option.value ~default: now created_at;
    modified_at = Option.value ~default: now modified_at;
  }

let update ?created_at ?modified_at meta = {
  created_at = Option.value ~default: meta.created_at created_at;
  modified_at = Option.value ~default: meta.modified_at modified_at;
}
