type t = | [@@deriving eq, yojson]
let f _ = assert false

let show _ = "<void>"
let pp fmt x = Format.pp_print_string fmt (show x)
