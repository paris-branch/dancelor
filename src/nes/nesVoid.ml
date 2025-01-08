type t = |

let f _ = assert false

let to_yojson = f
let of_yojson _ = Error "there are no void values"
