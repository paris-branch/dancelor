type t =
  Private | Public
[@@deriving show, yojson]

let default = Private
