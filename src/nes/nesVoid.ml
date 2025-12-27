type t = |

(** Since there are no void values, they can be converted to anything. *)
let f _ = assert false

let to_biniou = f
let of_biniou_exn = Ppx_deriving_biniou_runtime.could_not_convert "Void.of_biniou"

let to_yojson = f
let of_yojson _ = Error "there are no void values"
